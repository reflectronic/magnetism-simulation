namespace Simulation

open Silk.NET.Maths
open type Silk.NET.Maths.Vector3D
open type Vector3D<float>
open type System.Math

type Vector3 = Silk.NET.Maths.Vector3D<float>

module Calculate =
    let Mu = (4. * PI * 10e-7)

    //Magnetic Field
    let B (M, r) = (Mu / (4. * PI)) * (3. * (Dot(M, r) * r) / (r.Length ** 5.) - (M / (r.Length ** 3.)))

    //Potential
    let U (m, B) = Dot(-m, B)

    let torque (m, B) = Cross(m, B)

    type Dimension = 
        | X 
        | Y 
        | Z

    let force (position: Vector3D<float>, m, M) = 
        let inline product upperBound exclusions fn =
            let mutable partialProduct = 1.
            for i = 0 to upperBound do
                if not (exclusions |> Array.contains i ) then
                    partialProduct <- partialProduct * fn i
                else 
                    ()
            partialProduct

            (*[| 0..upperBound |]
            |> Array.except exclusions 
            |> Array.map (fun i -> fn i) 
            |> Array.fold (fun partialProduct factor -> partialProduct * factor) 1*)

        let inline sum upperBound exclusions fn =
            let mutable partialSum = 0.
            for i = 0 to upperBound do
                if not (exclusions |> Array.contains i) then
                    partialSum <- partialSum + fn i
                else 
                    ()
            partialSum

            (*[| 0..upperBound |]
            |> Array.except exclusions
            |> Array.sumBy (fun i -> fn i)*)

        // We use a polynomial interpolation to estimate the gradient of the potentials.
        // The Lagrange form of the interpolation is:
        //        𝑛
        // 𝐿(𝑥) = Σ  yⱼ * 𝑙ⱼ(𝑥).
        //       𝑗=0
        //
        // To find the gradient, we take the derivative of the Lagrange form, so the formula for gradient is:
        //         𝑛
        // 𝐿'(𝑥) = Σ = yⱼ * 𝑙ⱼ'(𝑥)
        //        𝑗=0 
        //
        // 𝑙ⱼ'(𝑥) represents the derivative of the Lagrange basis polynomial, given as:
        //         𝑛         𝑛
        // 𝑙ⱼ'(𝑥) = Σ        (Π (𝑥 - 𝑥ₗ))
        //        𝑘=0, 𝑘≠𝑗   𝑙=0, 𝑙≠𝑘, 𝑙≠𝑗
        //       -----------------------
        //                 𝑛
        //                 Π (𝑥ⱼ - 𝑥ₖ)
        //                𝑘=0, 𝑘≠𝑗 

        let l' (j, n, dx: float) = 
            assert(n = 2) 
            let x = [| -dx; 0; dx |]
            let numerator = (sum n (Array.singleton j) 
                (fun k -> 
                    product n [| k; j |] (fun l -> 0. - x.[l]))) 
            let denominator = product n (Array.singleton j) (fun k -> x.[j] - x.[k]);
            float numerator / float denominator
        
        let L' (yVals, dx) = 
            yVals 
            |> Array.mapi (fun i y -> y * float (l'(i, yVals.Length - 1, dx))) 
            |> Array.sum
                    
        let approximateGradient (pos: Vector3, dimension) =
            let currentPosition = match dimension with 
                                  | X -> pos.X
                                  | Y -> pos.Y
                                  | Z -> pos.Z
            
            let dx = 0.0001 // I made this smaller for more acccuracy
            let positionGradient = [| currentPosition - dx; currentPosition; currentPosition + dx |]

            let radii = positionGradient |> Array.map (fun p -> 
                match dimension with
                | X -> Vector3(p, pos.Y, pos.Z)
                | Y -> Vector3(pos.X, p, pos.Z)
                | Z -> Vector3(pos.X, pos.Y, p))

            let potentials = radii |> Array.map (fun r -> U(m, B(M, r)))

            L'(potentials, dx)

        -Vector3(approximateGradient(position, X), approximateGradient(position, Y), approximateGradient(position, Z))

    [<Literal>]
    let Radius = 0.01;

    [<Literal>]
    let Mass = 0.003;

    [<Struct>]
    type SimulatedObject = { PreviousPosition: Vector3; Position: Vector3; Velocity: Vector3; AngularVelocity: Vector3; MagneticMoment: Vector3; Mass: float }

    let rec runSimulation (M, simulatedObjects, dt, momentOfInertia: System.Func<int, float>, gamma: System.Func<int, float>, callback: System.Func<bool>) =
        let updatingMap mapping (array: 'T[]) = 
            for i = 0 to array.Length - 1 do 
                array.[i] <- mapping (i, array.[i])

        let inline delta (initial: Vector3, acceleration: Vector3) : Vector3 =
            (initial * dt) + (acceleration * (dt ** 2.) / 2.)

        simulatedObjects |> updatingMap (fun (i, o) -> 
                let magneticForce = force(o.Position, o.MagneticMoment, M)

                let inline angleBetween (a, b) = Acos(Dot(a, b) / (a.Length * b.Length))

                let xi = Abs(angleBetween(o.MagneticMoment, magneticForce))

                let F_ym = Abs(
                    if xi < (PI / 9.) then 
                        Cos(xi) * PI * (Radius ** 2) * 0.0004
                    else
                        Cos(xi) * Radius * 0.0002
                )

                let alpha = -0.80

                let F_net = -alpha * magneticForce + alpha * (Dot(magneticForce, Normalize(o.MagneticMoment)) * o.MagneticMoment) + magneticForce

                let force = if magneticForce.Length < F_ym then Zero else F_net - F_net / F_net.Length * F_ym * Cos(xi)

                let forceDrag = gamma.Invoke(i) * o.Velocity;
                
                let acceleration = (force + forceDrag) / o.Mass
                
                //if (force |> thresholdForce) then
                //    let acceleration = (force + forceDrag) / o.Mass
                //else 
                //    let acceleration = Zero

                // This isn't fully correct
                // Things to add:
                // - If movement is sharper than at threshold angle, movement goes along 
                //   previous trajectory
                //   - We can draw a "line" and if the angle is too sharp we snap the trajectory to it 
                //     by a certain amount (the projection, probably)
                //   - We can also create "tunnel cylinders" to demarkate the torn medium and diretion, and use those
                
                let torque = torque(o.MagneticMoment, B(M, o.Position))

                let coefficient = ((0.01 ** 2) * o.AngularVelocity.Length) / (4. * PI * 1.004e-1);
                let angularDrag = o.AngularVelocity * coefficient
                //let angularAcceleration = (torque - Zero) / momentOfInertia.Invoke(i)
                let angularAcceleration = (torque - angularDrag) / momentOfInertia.Invoke(i)

                let dtheta = delta(o.AngularVelocity, angularAcceleration)

                let len = o.MagneticMoment.Length;

                { o with
                    PreviousPosition = o.Position
                    Position = o.Position + delta(o.Velocity, acceleration)
                    Velocity = o.Velocity + acceleration * dt
                    MagneticMoment = Transform(o.MagneticMoment, Quaternion.CreateFromAxisAngle(Normalize(dtheta), dtheta.Length))
                    AngularVelocity = o.AngularVelocity + angularAcceleration * dt
                }
            )

        if callback.Invoke() then runSimulation (M, simulatedObjects, dt, momentOfInertia, gamma, callback) else ()

        
