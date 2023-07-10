namespace Simulation

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Vectors
open LanguagePrimitives
open NonStructuralComparison

open type System.Math

type PathFace = 
    {
        Position: Vector3<m>
        Radius: float<m>
    }

[<Struct>]
type SimulatedObject = 
    { 
        Position: Vector3<m>;
        Radius: float<m>;
        Path: PathFace list
    }

    static member op_Equality (left: SimulatedObject, right: SimulatedObject) =
        FSharp.Core.Operators.(=) left right

    static member op_Inequality (left: SimulatedObject, right: SimulatedObject) =
        FSharp.Core.Operators.(<>) left right

module Calculate =
    let inline internal pow5 v = v * v * v * v * v
    let inline internal pow4 v = v * v * v * v
    let inline internal cubed v = v * v * v
    let inline internal squared v = v * v

    let Mu_0 = FloatWithMeasure<H/m> (4. * PI * 1e-7)

    let H (M: Vector3<A m^2>, r: Vector3<m>) = (1. / (4. * PI)) * (3. * (Dot(M, r) * r) / (Length(r) |> pow5) - (M / (Length(r) |> cubed)))

    let B (M: Vector3<A m^2>, r): Vector3<T> = Mu_0 * H(M, r)

    let inline U (m: Vector3<A m^2>, B: Vector3<T>) = Dot(m, B)
        
    let inline torque (m: Vector3<A m^2>, B: Vector3<T>) = Cross(m, B)

    let magneticForce (r: Vector3<m>, m: Vector3<A m^2>, M: Vector3<A m^2>): Vector3<N> = 
        let f = (Dot(m, r) * M) + (Dot(M, r) * m) + (Dot(m, M) * r) - ((5. * Dot(m, r) * Dot(M, r)) / (Length(r) |> squared)) * r
        (3. * Mu_0) / (4. * PI * (Length(r) |> pow5)) * f

    let BFromPositions (pair, shouldExpand, fieldStrength: float<T>) =
        let struct (l, s) = pair
        let fieldDirection = 
            let positionDelta = l.Position - s.Position
            let flippedDelta = if shouldExpand then Cross(positionDelta, Vector3(0., 0., 1.)) else positionDelta
            Normalize(flippedDelta)
        fieldDirection * fieldStrength

open Calculate
open FSharp.Collections.ParallelSeq

[<Struct>]
type SimulationResult<'a, 'b> =
    | ContinueSimulation of field: Vector3<T> * state: 'a
    | EndSimulation of result: 'b

module Simulation =
    let rec run (pair: struct (SimulatedObject * SimulatedObject), 
                 dt: float<s>, 
                 externalBField: Vector3<T>,
                 isExpanding: bool)
                 state
                 callback =
        let struct(l, s) = pair

        let distance pt1 pt2 = Length(pt1 - pt2)

        let angleBetween cylDir magneticForce = acos(Dot(cylDir, magneticForce) / (Length(cylDir) * Length(magneticForce)))

        let minBy projection v1 v2 = 
            if projection v1 < projection v2 then v1, v2 else v2, v1

        let cylinders o = 
            let rec makeCyls (pt: PathFace) rest cyls =
                // pt was collected after next
                match rest with 
                | next::rest ->
                    let canIntersect (pt: PathFace) = 
                        pt.Radius + o.Radius > distance pt.Position o.Position

                    if canIntersect pt && canIntersect next then
                        match rest with 
                        | next::rest -> let cyl = (pt.Position, next.Position) in makeCyls next rest (cyl::cyls)
                        | _ -> cyls
                    else
                        makeCyls next rest cyls
                | _ -> cyls
                    
            match o.Path with 
            | pt::rest -> makeCyls pt rest []
            | _ -> []

        let trySkip l = 
            match l with
            | [] -> []
            | _ -> List.tail l

        let cylinder o magneticForce =
            cylinders o
            |> trySkip
            |> PSeq.filter (fun c ->
                let toSphere = distance o.Position
                let (startPt, endPt) = c

                let (closer, further) = minBy toSphere startPt endPt
                let planeNormal = Normalize(further - closer)

                let toPoint = o.Position - closer
                let dist = Dot(toPoint, planeNormal)
                let projectedPoint = o.Position - dist * planeNormal

                let centerToPlanarCenter = Length(projectedPoint - o.Position)
                let l = o.Radius * sin(acos(centerToPlanarCenter / o.Radius))
                centerToPlanarCenter < o.Radius && Length(projectedPoint - closer) <= o.Radius + l)
            |> PSeq.map (fun c ->
                let (pt1, pt2) = c
                let cylDir = pt1 - pt2

                let betweenForce = angleBetween magneticForce
                let thetaForceCyl = betweenForce cylDir
                let thetaForceCap = min (betweenForce (pt1 - o.Position)) (betweenForce (pt2 - o.Position))
                c, thetaForceCap, if (thetaForceCyl > (PI / 2.0)) then PI - thetaForceCyl else thetaForceCyl)
            |> PSeq.sortBy (fun c -> let (_, _, thetaForceCyl) = c in thetaForceCyl)
            |> PSeq.tryFind (fun c -> let (cyl, thetaForceCap, thetaForceCyl) = c in thetaForceCap < 1.0 && thetaForceCyl < 1.0)
            |> Option.map (fun c -> let (cyl, _, _) = c in cyl)

        let magneticMoment (o: SimulatedObject) =
            let volume = (4./3.) * PI * (o.Radius |> cubed)
            (externalBField / Mu_0) * volume / 3.

        let lMagneticForce = magneticForce(l.Position - s.Position, magneticMoment(l), magneticMoment(s))
        let sMagneticForce = -lMagneticForce

        let forwardThreshold = 0.012<N>
        let sigma = forwardThreshold / ((PI / 4.) * ((3.2e-3<m>) |> squared))

        let backwardsThreshold = 0.008<N>
        let lambda = backwardsThreshold / (3.2e-3<m>)

        let gamma = 0.004<N> / (3.2e-3<m> * 0.5e-3<m/s>)

        let lCyl = cylinder l lMagneticForce
        let sCyl = cylinder s sMagneticForce

        let diameter (o: SimulatedObject) = o.Radius * 2.

        let tearingVelocityMagnitude (force: Vector3<N>, threshold: float<N>, diameter: float<m>) = 
            let mag = (Length(force) - threshold) / (gamma * diameter)
            if mag >= 0.<m/s> && (Abs(float force.X) >= float threshold) then
                mag
            else
                0.<m/s>

        let project a b v = 
            let ab = b - a
            let abNorm = Normalize(ab)
            Dot(v, abNorm) * abNorm

        let projectForce cyl force =
            match cyl with
            | Some cyl -> let (pt1, pt2) = cyl in project pt2 pt1 force
            | None -> force

        let lVelocity = 
            let largeThreshold = 
                if lCyl.IsNone then 
                    sigma * (diameter(l) |> squared)
                else
                    lambda * diameter(l)

            let projectedMagneticForce = projectForce lCyl lMagneticForce
            Normalize(projectedMagneticForce) * tearingVelocityMagnitude (projectedMagneticForce, largeThreshold, diameter(l)) 

        let sVelocity = 
            let smallThreshold =
                if sCyl.IsNone then 
                    sigma * (diameter(s) |> squared)
                else
                    lambda * diameter(s)

            let projectedMagneticForce = projectForce sCyl sMagneticForce
            Normalize(projectedMagneticForce) * tearingVelocityMagnitude (sMagneticForce, smallThreshold, diameter(s))

        let projectPoint a b p =
            let ab = b - a
            a + Dot(p - a, ab) / Dot(ab, ab) * (ab)

        let lPos = lVelocity * dt +
            match lCyl with 
            | Some cyl -> let (pt1, pt2) = cyl in projectPoint pt1 pt2 l.Position
            | None -> l.Position

        let sPos = sVelocity * dt +
            match sCyl with
            | Some cyl -> let (pt1, pt2) = cyl in projectPoint pt1 pt2 s.Position
            | None -> s.Position

        let pathFace o p =
            { Position = p; Radius = o.Radius }

        let pair = struct(
                { l with Position = lPos;
                            Path = (pathFace l lPos)::l.Path }, 
                { s with Position = sPos;
                            Path = (pathFace s sPos)::s.Path })

        // If we are currently expanding, we should start contracting when the large ball can no longer overcome the threshold.
        // If we are currently contracting, we should start expanding when the large ball begins to overcome its threshold.
        // If the external field is zero, the balls cannot be moving, so we cannot make a determination about the direction of the fields. Carry the previous state forward.
        let shouldExpand = if externalBField <> Vector3.Zero then Length(lVelocity) <> 0.<m/s> else isExpanding
        // let shouldExpand = true
    
        match callback struct(pair, shouldExpand, struct(lCyl, sCyl, state)) with
        | ContinueSimulation(field, state) -> run (pair, dt, field, shouldExpand) state callback
        | EndSimulation(result) -> result
