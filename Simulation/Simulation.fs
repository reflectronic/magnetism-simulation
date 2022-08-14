namespace Simulation

open Silk.NET.Maths
open type Silk.NET.Maths.Vector3D
open type Vector3D<float>
open type System.Math

type Vector3 = Silk.NET.Maths.Vector3D<float>

[<Struct>]
type SimulatedObject = 
    { 
        PreviousPosition: Vector3;
        Position: Vector3;
        Velocity: Vector3;
        AngularVelocity: Vector3;
        MagneticMoment: Vector3;
        Mass: float;
        Radius: float
    }

module Calculate =
    let Mu = (4. * PI * 10e-7)

    //Magnetic Field
    let B (M, r) = (Mu / (4. * PI)) * (3. * (Dot(M, r) * r) / (r.Length ** 5.) - (M / (r.Length ** 3.)))

    //Potential
    let inline U (m, B) = Dot(-m, B)

    let inline torque (m, B) = Cross(m, B)

    type Dimension = 
        | X 
        | Y 
        | Z

    let force (position: Vector3D<float>, m, M) = 
        let inline product upperBound exclusions fn =
            let mutable partialProduct = 1.
            for i = 0 to upperBound do
                let struct (exclusion1, exclusion2) = exclusions

                if exclusion1 <> i && exclusion2 <> i then
                    partialProduct <- partialProduct * fn i
                else 
                    ()
            partialProduct

        let inline sum upperBound exclusion fn =
            let mutable partialSum = 0.
            for i = 0 to upperBound do
                if i <> exclusion then
                    partialSum <- partialSum + fn i
                else 
                    ()
            partialSum

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

        // We always take the derivative at 𝑥 = 0.
        let l' (j, dx: float) = 
            let n = 2
            let x = Vector3(-dx, 0, dx)
            let numerator = (sum n j 
                (fun k -> product n (k, j) (fun l -> 0. - x.[l]))) 

            let denominator = product n (j, -1) (fun k -> x.[j] - x.[k]);
            float numerator / float denominator
        
        let L' (yVals, dx) = 
            let inline vectorMap fn (v: Vector3) =
                Vector3(fn 0 v.X, fn 1 v.Y, fn 2 v.Z)

            let inline vectorSum (v: Vector3) = 
                v.X + v.Y + v.Z

            yVals 
            |> vectorMap (fun i y -> y * float (l'(i, dx))) 
            |> vectorSum
                    
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

            L'(Vector3(potentials.[0], potentials.[1], potentials.[2]), dx)

        -Vector3(approximateGradient(position, X), approximateGradient(position, Y), approximateGradient(position, Z))

    let rec runSimulation (pair: byref<struct (SimulatedObject * SimulatedObject)>, 
                           dt: float, 
                           callback: System.Func<bool>) =
        let inline delta (initial: Vector3, acceleration: Vector3) : Vector3 =
            (initial * dt) + (acceleration * (dt * dt) / 2.)
            
        // let inline angleBetween (a, b) = Acos(Dot(a, b) / (a.Length * b.Length))

        let struct (o1, o2) = pair
            
        // The net force applied on both objects is the same (Newton's third law).
        // We can calculate the force applied to o1 as a result of o2's magnetic field,
        // and apply the opposite of that force to o2.
        // Since the radius of both balls is not the same, however, the threshold force
        // will need to be calculated independently for both.

        let unitCubeToSphere (p: Vector3) = 
            p * Vector3(
                    Sqrt(1. - p.Y ** 2 / 2. - p.Z ** 2 / 2. + p.Y ** 2 * p.Z ** 2 / 3.),
                    Sqrt(1. - p.X ** 2 / 2. - p.Z ** 2 / 2. + p.X ** 2 * p.Z ** 2 / 3.),
                    Sqrt(1. - p.X ** 2 / 2. - p.Y ** 2 / 2. + p.X ** 2 * p.Y ** 2 / 3.))

        // Arbitrary value that balances the density of points with processing time.
        // For a sphere with a radius of 0.01, this should provide us with about 10 points
        // from one point on the sphere to its opposite point.
        let pointGap = o1.Radius * 2. / 5.

        let pointsLength = Round(o1.Radius * 2. / pointGap, System.MidpointRounding.AwayFromZero) |> int

        let matrixLength = pointsLength * pointsLength * pointsLength

        let magneticForce = 
            Array.Parallel.init matrixLength (fun i -> 
                let x = i % pointsLength
                let y = i / pointsLength % pointsLength
                let z = i / (pointsLength * pointsLength) % pointsLength

                let indexToPos i = -o1.Radius + pointGap * i
                let unitPosition = Vector3(indexToPos x, indexToPos y, indexToPos z) |> unitCubeToSphere
                let p = unitPosition * o1.Radius + o1.Position
                force(p - o2.Position, o1.MagneticMoment / float matrixLength, o2.MagneticMoment))
             |> Seq.sum

        let calculateBallDelta (magneticForce, o, otherObject) =
            let forceDrag = (6. * PI * o.Radius * 1.002e-3) * o1.Velocity;
            let acceleration: Vector3 = (magneticForce + forceDrag) / o.Mass

            // This isn't fully correct
            // Things to add:
            // - If movement is sharper than at threshold angle, movement goes along 
            //   previous trajectory
            //   - We can draw a "line" and if the angle is too sharp we snap the trajectory to it 
            //     by a certain amount (the projection, probably)
            //   - We can also create "tunnel cylinders" to demarkate the torn medium and diretion, and use those
                
            let torque = torque(o.MagneticMoment, B(otherObject.MagneticMoment, o.Position - otherObject.Position))

            let coefficient = ((0.01 * 0.01) * o.AngularVelocity.Length) / (4. * PI * 1.004e-1);
            let angularDrag = o.AngularVelocity * coefficient
            let angularAcceleration = (torque - angularDrag) / ((2. / 5.) * o.Mass * (o.Radius * o.Radius))

            let dtheta = delta(o.AngularVelocity, angularAcceleration)

            { o with
                PreviousPosition = o.Position
                Position = o.Position + delta(o.Velocity, acceleration)
                Velocity = o.Velocity + acceleration * dt
                MagneticMoment = Transform(o.MagneticMoment, Quaternion.CreateFromAxisAngle(Normalize(dtheta), dtheta.Length))
                AngularVelocity = o.AngularVelocity + angularAcceleration * dt
            }

        pair <- struct(calculateBallDelta (magneticForce, o1, o2), calculateBallDelta (-magneticForce, o2, o1))
        
        if callback.Invoke() then runSimulation (&pair, dt, callback) else ()
