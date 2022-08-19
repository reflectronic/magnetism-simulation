namespace Simulation

open System.Numerics
open type System.Numerics.Vector3
open type System.MathF

[<Struct>]
type SimulatedObject = 
    { 
        PreviousPosition: Vector3;
        Position: Vector3;
        Velocity: Vector3;
        AngularVelocity: Vector3;
        MagneticMoment: Vector3;
        Mass: float32;
        Radius: float32
    }

module Calculate =
    let Mu = (4f * PI * 10e-7f)

    // The F# pow operator calls into the CRT pow function for `float`.
    // B is in the innermost loop of the simulation, so pow often shows up very hot in profiles.
    // Manually writing out the multiplications avoids this problem.
    let inline pow5 v = v * v * v * v * v
    let inline cubed v = v * v * v
    let inline squared v = v * v

    let B (M, r) = (Mu / (4f * PI)) * (3f * (Dot(M, r) * r) / (r.Length() |> pow5) - (M / (r.Length() |> cubed)))

    let inline U (m, B) = Dot(-m, B)

    let inline torque (m, B) = Cross(m, B)

    [<Struct>]
    type private Dimension = 
        | X
        | Y
        | Z

    let force (position: Vector3, m, M) = 
        let inline product upperBound exclusions fn =
            let mutable partialProduct = 1f
            for i = 0 to upperBound do
                let struct (exclusion1, exclusion2) = exclusions
                if exclusion1 <> i && exclusion2 <> i then
                    partialProduct <- partialProduct * fn i

            partialProduct

        let inline sum upperBound exclusion fn =
            let mutable partialSum = 0f
            for i = 0 to upperBound do
                if i <> exclusion then
                    partialSum <- partialSum + fn i
                
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
        let l' (j, dx) = 
            let n = 2
            let x = Vector3(-dx, 0f, dx)
            let numerator = (sum n j 
                (fun k -> product n (k, j) (fun l -> 0f - x.[l]))) 

            let denominator = product n (j, -1) (fun k -> x.[j] - x.[k]);
            float32 numerator / float32 denominator
           
        let inline project fn v = 
            let struct(x, y, z) = v
            struct(fn 0 x, fn 1 y, fn 2 z)

        let L' (yVals, dx) = 
            let inline sum v =
                let struct(x, y, z) = v
                x + y + z

            yVals 
            |> project (fun i y -> y * float32 (l'(i, dx))) 
            |> sum
          
        let approximateGradient (pos: Vector3, dimension) =
            let currentPosition = match dimension with 
                                  | X -> pos.X
                                  | Y -> pos.Y
                                  | Z -> pos.Z
            
            let dx = 0.0001f
            let positionGradient = struct(currentPosition - dx, currentPosition, currentPosition + dx)

            let potentials = positionGradient |> project (fun _ p -> 
                let radius = match dimension with
                             | X -> Vector3(p, pos.Y, pos.Z)
                             | Y -> Vector3(pos.X, p, pos.Z)
                             | Z -> Vector3(pos.X, pos.Y, p)
                U(m, B(M, radius)))

            L'(potentials, dx)

        -Vector3(approximateGradient(position, X), approximateGradient(position, Y), approximateGradient(position, Z))

    let rec runSimulation (pair: byref<struct (SimulatedObject * SimulatedObject)>, 
                           dt: float32, 
                           externalField: inref<Vector3>,
                           callback: System.Func<bool>) =
        let inline delta (initial: Vector3, acceleration: Vector3) : Vector3 =
            (initial * dt) + (acceleration * (dt * dt) / 2f)
            
        // let inline angleBetween (a, b) = Acos(Dot(a, b) / (a.Length * b.Length))

        let struct(o1, o2) = pair
            
        // The net force applied on both objects is the same (Newton's third law).
        // We can calculate the force applied to o1 as a result of o2's magnetic field,
        // and apply the opposite of that force to o2.
        // Since the radius of both balls is not the same, however, the threshold force
        // will need to be calculated independently for both.

        let unitCubeToSphere (p: Vector3) = 
            p * Vector3(
                    Sqrt(1f - p.Y ** 2f / 2f - p.Z ** 2f / 2f + p.Y ** 2f * p.Z ** 2f / 3f),
                    Sqrt(1f - p.X ** 2f / 2f - p.Z ** 2f / 2f + p.X ** 2f * p.Z ** 2f / 3f),
                    Sqrt(1f - p.X ** 2f / 2f - p.Y ** 2f / 2f + p.X ** 2f * p.Y ** 2f / 3f))

        // Arbitrary value that balances the density of points with processing time.
        // For a sphere with a radius of 0.01, this should provide us with about 10 points
        // from one point on the sphere to its opposite point.
        let pointGap = 0f//o1.Radius * 2f / 5f

        let pointsLength = 1//Round(o1.Radius * 2f / pointGap, System.MidpointRounding.AwayFromZero) |> int

        let matrixLength = pointsLength * pointsLength * pointsLength
        
        let magneticForce = 
            Array.init matrixLength (fun i -> 
                let x = i % pointsLength |> float32
                let y = i / pointsLength % pointsLength  |> float32
                let z = i / (pointsLength * pointsLength) % pointsLength  |> float32

                let indexToPos i = -o1.Radius + pointGap * i
                let unitPosition = Vector3(indexToPos x, indexToPos y, indexToPos z) //|> unitCubeToSphere 
                if unitPosition.Length() <= 1f then 
                    let p = unitPosition * o1.Radius + o1.Position
                    force(p - o2.Position, o1.MagneticMoment / float32 matrixLength, o2.MagneticMoment)
                else 
                    Zero)
            |> Array.sum

        let calculateBallDelta (magneticForce, o, otherObject, externalField) =
            let forceDrag = (6f * PI * o.Radius * 1.002e-3f) * o1.Velocity;
            let acceleration: Vector3 = (magneticForce + forceDrag) / o.Mass

            // This isn't fully correct
            // Things to add:
            // - If movement is sharper than at threshold angle, movement goes along 
            //   previous trajectory
            //   - We can draw a "line" and if the angle is too sharp we snap the trajectory to it 
            //     by a certain amount (the projection, probably)
            //   - We can also create "tunnel cylinders" to demarkate the torn medium and diretion, and use those
                
            let torque = torque(o.MagneticMoment, B(otherObject.MagneticMoment, o.Position - otherObject.Position) + externalField)

            let coefficient = ((0.01f * 0.01f) * o.AngularVelocity.Length()) / (4f * PI * 1.004e-1f);
            let angularDrag = o.AngularVelocity * coefficient
            let angularAcceleration = (torque - angularDrag) / ((2f / 5f) * o.Mass * (o.Radius * o.Radius))

            let dtheta = delta(o.AngularVelocity, angularAcceleration)

            { o with
                PreviousPosition = o.Position
                Position = o.Position + delta(o.Velocity, acceleration)
                Velocity = o.Velocity + acceleration * dt
                MagneticMoment = Transform(o.MagneticMoment, Quaternion.CreateFromAxisAngle(Normalize(dtheta), dtheta.Length()))
                AngularVelocity = o.AngularVelocity + angularAcceleration * dt
            }

        pair <- struct(calculateBallDelta (magneticForce, o1, o2, externalField), calculateBallDelta (-magneticForce, o2, o1, externalField))
        
        if callback.Invoke() then runSimulation (&pair, dt, &externalField, callback) else ()
