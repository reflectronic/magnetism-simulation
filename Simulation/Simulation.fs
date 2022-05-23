namespace Simulation

open Silk.NET.Maths
open type Silk.NET.Maths.Vector3D
open type Silk.NET.Maths.Vector3D<float>
open type System.Math

type Vector3 = Silk.NET.Maths.Vector3D<float>

module Calculate =
    let Mu = (4. * PI * 10e-7)

    let B (M, r) = (Mu / (4. * PI)) * (3. * (Dot(M, r) * r) / (r.Length ** 5.) - (M / (r.Length ** 3.)))

    let U (m, B) = Dot(-m, B)

    let torque (m, B) = Cross(m, B)

    type Dimension = 
        | X = 0 
        | Y = 1 
        | Z = 2

    let force (position: Vector3D<float>, m, M) = 
        let inline product upperBound exclusions fn = 
            let mutable partialProduct = 1
            for i = 0 to upperBound do
                if not (Array.contains i exclusions) then
                    partialProduct <- partialProduct * fn i
                else 
                    ()
            partialProduct
                    
        (*
            seq { 0..upperBound } 
            |> Seq.toArray
            |> Array.except exclusions 
            |> Array.map (fun i -> fn i) 
            |> Array.fold (fun partialProduct factor -> partialProduct * factor) 1
        *)

        
        let inline sum upperBound exclusions fn =
            let mutable partialSum = 1
            for i = 0 to upperBound do
                if not (Array.contains i exclusions) then
                    partialSum <- partialSum + fn i
                else 
                    ()
            partialSum

        (*
            seq { 0..upperBound }
            |> Seq.toArray
            |> Array.except exclusions
            |> Array.sumBy (fun i -> fn i
         *)

        let l' j n = 
            let numerator = (sum n (Array.singleton j) 
                (fun k -> 
                    product n [| k; j |] (fun l -> (n / 2) - l))) 
            let denominator = product n (Array.singleton j) (fun k -> j - k);
            float numerator / float denominator
        
        let L' yVals = 
            yVals 
            |> Array.mapi (fun i y -> y * float (l' i (yVals.Length - 1))) 
            |> Array.sum
                    
        let approximateGradient (pos: Vector3, dimension) =
            let currentPosition = match dimension with 
                                  | Dimension.X -> pos.X
                                  | Dimension.Y -> pos.Y
                                  | Dimension.Z -> pos.Z

            let positionGradient = [| currentPosition - 0.001; currentPosition; currentPosition + 0.001 |]

            let radii = positionGradient |> Array.map (fun p -> 
                match dimension with
                | Dimension.X -> Vector3(p, pos.Y, pos.Z)
                | Dimension.Y -> Vector3(pos.X, p, pos.Z)
                | Dimension.Z -> Vector3(pos.X, pos.Y, p))

            let potentials = radii |> Array.map (fun r -> U(m, B(M, r)))

            L' potentials

        -Vector3(approximateGradient(position, Dimension.X), approximateGradient(position, Dimension.Y), approximateGradient(position, Dimension.Z))


    [<Struct>]
    type SimulatedObject = { Position: Vector3; Velocity: Vector3; AngularVelocity: Vector3; MagneticMoment: Vector3; Mass: float }


    let rec runSimulation (M, simulatedObjects, dt, momentOfInertia: System.Func<int, float>, gamma: System.Func<int, float>, callback: System.Func<bool>) =
        let updatingMap mapping (array: 'T[]) = 
            for i = 0 to array.Length - 1 do 
                array.[i] <- mapping (i, array.[i])

        let inline delta (initial: Vector3, acceleration: Vector3) : Vector3 =
            (initial * dt) + (acceleration * (dt ** 2.) / 2.)

        simulatedObjects |> updatingMap (fun (i, o) -> 

                let magneticForce = force(o.Position, o.MagneticMoment, M)

                let force = if o.Velocity = Zero && magneticForce.Length < 0. then Zero else magneticForce

                let forceDrag = gamma.Invoke(i) * o.Velocity;
                let acceleration = (force + forceDrag) / o.Mass
                
                let torque = torque(o.MagneticMoment, B(M, o.Position))

                let coefficient = ((0.01 ** 2) * o.AngularVelocity.Length) / (4. * PI * 1.004e-1);
                let angularDrag = o.AngularVelocity * coefficient
                let angularAcceleration = (torque - angularDrag) / momentOfInertia.Invoke(i)

                let dtheta = delta(o.AngularVelocity, angularAcceleration)

                let len = o.MagneticMoment.Length;

                { o with
                    Position = o.Position + delta(o.Velocity, acceleration)
                    Velocity = o.Velocity + acceleration * dt
                    MagneticMoment = Transform(o.MagneticMoment, Quaternion.CreateFromAxisAngle(Normalize(dtheta), dtheta.Length))
                    AngularVelocity = o.AngularVelocity + angularAcceleration * dt
                }
            )

        if callback.Invoke() then runSimulation (M, simulatedObjects, dt, momentOfInertia, gamma, callback) else ()

        
