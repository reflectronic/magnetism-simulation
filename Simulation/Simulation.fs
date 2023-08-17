namespace Simulation

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Vectors
open LanguagePrimitives
open NonStructuralComparison

open System
open type System.Double

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

    let isBetween (lower, upper) x = lower <= x && x <= upper
    let distance pt1 pt2 = Length(pt1 - pt2)

    let cosBetween a b =
        let res = Clamp(Dot(a, b) / (Length(a) * Length(b)), -1., 1.)
        if IsNaN res then 0. else res

    let angleBetween a b = Acos(cosBetween a b)

    let Mu_0 = FloatWithMeasure<H/m> (4. * Pi * 1e-7)

    let H (M: Vector3<A m^2>, r: Vector3<m>) = (1. / (4. * Pi)) * (3. * (Dot(M, r) * r) / (Length(r) |> pow5) - (M / (Length(r) |> cubed)))

    let B (M: Vector3<A m^2>, r): Vector3<T> = Mu_0 * H(M, r)

    let inline U (m: Vector3<A m^2>, B: Vector3<T>) = Dot(m, B)
        
    let inline torque (m: Vector3<A m^2>, B: Vector3<T>) = Cross(m, B)

    let magneticForce (r: Vector3<m>, m: Vector3<A m^2>, M: Vector3<A m^2>): Vector3<N> = 
        let f = (Dot(m, r) * M) + (Dot(M, r) * m) + (Dot(m, M) * r) - ((5. * Dot(m, r) * Dot(M, r)) / (Length(r) |> squared)) * r
        (3. * Mu_0) / (4. * Pi * (Length(r) |> pow5)) * f

    let BFromPositions (pair, shouldExpand, (theta: float), fieldStrength: float<T>) =
        let struct (l, s) = pair
        let fieldDirection = 
            let positionDelta = l.Position - s.Position
            let flippedDelta = 
                if shouldExpand then 
                    let rotationQuat = 
                        Numerics.Quaternion.CreateFromAxisAngle(Numerics.Vector3(0.f, 0.f, 1.f), Single.Pi / 2.f + (float32 theta))

                    Numerics.Vector3.Transform(NumericsVector positionDelta, rotationQuat) |> SimulationVector
                else 
                    positionDelta
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
                 iter
                 callback =
        let struct(l, s) = pair

        let cylinders o p1 p2 = 
            let rec makeCyls (endPt: PathFace) rest cyls =
                let canIntersect (pt: PathFace) = 
                    pt.Radius + o.Radius > distance pt.Position o.Position

                match rest with 
                | startPt::rest ->
                    if canIntersect endPt || canIntersect startPt then
                        let cyl = (endPt.Position, startPt.Position, endPt.Radius)
                        makeCyls startPt rest (cyl::cyls)
                    else
                        makeCyls startPt rest cyls
                | _ -> cyls
                    
            let cylMatches = 
                (match p1 with 
                | pt::rest -> makeCyls pt rest []
                | _ -> []),
                (match p2 with 
                | pt::rest -> makeCyls pt rest []
                | _ -> [])

            cylMatches ||> List.append

        let trySkip l = 
            match l with
            | [] -> []
            | _ -> List.tail l

        let closestPointToMagneticForce o cyl magneticForce =
            let betweenForceDirecton pt = angleBetween magneticForce (pt - o.Position)
            let (p1, p2, _) = cyl
            
            let p1ToForce = betweenForceDirecton p1
            let p2ToForce = betweenForceDirecton p2

            if p1ToForce > p2ToForce then
                p1, p1ToForce
            else
                p2, p2ToForce


        let cylinder o oth magneticForce =
            cylinders o o.Path oth
            |> trySkip
            |> PSeq.filter (fun c ->
                let (endPt, startPt, cylRad) = c

                let n = endPt - startPt
                let a = startPt - o.Position
                let dist = Length(Cross(a, n)) / Length(n)

                dist < o.Radius + cylRad)
            |> PSeq.sortBy (fun c -> closestPointToMagneticForce o c magneticForce |> snd)
            |> PSeq.filter (fun c -> let (endPt, startPt, _) = c in angleBetween (endPt - startPt) magneticForce > (Pi / 4.))
            |> Seq.tryHead

        let magneticMoment (o: SimulatedObject) =
            let volume = (4./3.) * Pi * (o.Radius |> cubed)
            (externalBField / Mu_0) * volume / 3.

        let lMagneticForce = magneticForce(l.Position - s.Position, magneticMoment(l), magneticMoment(s))
        let sMagneticForce = -lMagneticForce

        let forwardThreshold = 0.012<N>
        let sigma = forwardThreshold / ((Pi / 4.) * ((3.2e-3<m>) |> squared))

        let backwardsThreshold = 0.0458<N>
        let lambda = backwardsThreshold / (3.2e-3<m>)

        let gamma = 0.004<N> / (3.2e-3<m> * 0.5e-3<m/s>)

        let diameter (o: SimulatedObject) = o.Radius * 2.

        let effectiveForce o cyl (magneticForce: Vector3<N>)  =
            let m1 = -0.80
            let m2 = 1.82

            let a cosPsi = 
                let psi = Acos(cosPsi)
                if psi |> isBetween (0., Pi / 2.) then 
                    0. 
                else if psi |> isBetween (31. * Pi / 36., Pi) then 
                    1. 
                else
                    m1 * (cosPsi |> squared) - m2 * cosPsi

            let cylidnerAnisotropyDirection cyl = closestPointToMagneticForce o cyl magneticForce |> fst

            let n, cosPsi =
                match cyl with 
                | Some cyl -> let n = Normalize(cylidnerAnisotropyDirection cyl) in n, cosBetween magneticForce n
                | None -> Normalize(magneticForce), 0.

            n, (1. - a(cosPsi)) * magneticForce + a(cosPsi) * (Dot(magneticForce, n)) * n

        let lCyl = cylinder l [] lMagneticForce
        let sCyl = cylinder s l.Path sMagneticForce

        let yieldForce effectiveForce anisotropy = 
            let cosXi = cosBetween effectiveForce anisotropy 
            let Fym cosXi = 
                let n1 = 0.0058<N>
                let n2 = -0.0138<N>
                let n3 = 0.0118<N>

                let xi = Acos(cosXi)
                if xi |> isBetween (0., Pi / 2.) then 
                    n3
                else
                    n1 * (cosXi |> squared) - n2 * cosXi + n3
            let Fym = Fym cosXi

            if Length(effectiveForce) < Fym then
                effectiveForce
            else
                Normalize(effectiveForce) * Fym

        let velocity o cyl magneticForce =
            let anisotropy, effectiveForce = effectiveForce o cyl magneticForce
            let yieldForce = yieldForce effectiveForce anisotropy

            let netForce = effectiveForce - yieldForce

            Normalize(netForce) * Length(netForce) / (gamma * (diameter o))

        let deltaSphere o cyl magneticForce =
            let velocity = velocity o cyl magneticForce
            let position = (velocity * dt) + o.Position
            let path = 
                match cyl with
                | None when iter % 50 = 0 -> { Position = position; Radius = o.Radius }::o.Path
                | _ -> o.Path

            velocity, { o with Position = position; Path = path }

        let largeVelocty, l = deltaSphere l lCyl lMagneticForce
        let _, s            = deltaSphere s sCyl sMagneticForce
        let pair = struct(l, s)

        // If we are currently expanding, we should start contracting when the large ball can no longer overcome the threshold.
        // If we are currently contracting, we should start expanding when the large ball begins to overcome its threshold.
        // If the external field is zero, the balls cannot be moving, so we cannot make a determination about the direction of the fields. Carry the previous state forward.
        let shouldExpand = if externalBField <> Vector3.Zero then Length(largeVelocty) <> 0.<m/s> else isExpanding
        // let shouldExpand = true
    
        match callback struct(pair, shouldExpand, struct(lCyl, sCyl, lMagneticForce, state)) with
        | ContinueSimulation(field, state) -> run (pair, dt, field, shouldExpand) state (iter + 1) callback
        | EndSimulation(result) -> result
