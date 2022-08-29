namespace Simulation

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Vectors
open LanguagePrimitives

open type System.Math

// type Vector3 = Silk.NET.Maths.Vector3D<float>
// type Quaternion = Silk.NET.Maths.Quaternion<float>

[<Struct>]
type SimulatedObject = 
    { 
        PreviousPosition: Vector3<m>;
        Position: Vector3<m>;
        Velocity: Vector3<m/s>;
        AngularVelocity: Vector3<1/s>;
        MagneticMoment: Vector3<A m^2>;
        Mass: float<kg>;
        Radius: float<m>
    }

module Calculate =
    let Mu_0 = FloatWithMeasure<H/m> (4. * PI * 10e-7)

    // The F# pow operator calls into the CRT pow function for `float`.
    // B is in the innermost loop of the simulation, so pow often shows up very hot in profiles.
    // Manually writing out the multiplications avoids this problem.
    let inline pow5 v = v * v * v * v * v
    let inline cubed v = v * v * v
    let inline squared v = v * v

    let H (M: Vector3<A m^2>, r: Vector3<m>) = (1. / (4. * PI)) * (3. * (Dot(M, r) * r) / (Length(r) |> pow5) - (M / (Length(r) |> cubed)))

    let B (M: Vector3<A m^2>, r): Vector3<T> = Mu_0 * H(M, r)

    let inline U (m: Vector3<A m^2>, B: Vector3<T>) = Dot(m, B)

    let inline torque (m: Vector3<A m^2>, B: Vector3<T>) = Cross(m, B)

    [<Struct>]  
    type private Dimension = 
        | X
        | Y
        | Z

    let magneticForce (position: Vector3<m>, m, M): Vector3<N> = 
        let inline product upperBound exclusions fn =
            let mutable partialProduct = 1.
            for i = 0 to upperBound do
                let struct (exclusion1, exclusion2) = exclusions
                if exclusion1 <> i && exclusion2 <> i then
                    partialProduct <- partialProduct * fn i

            partialProduct

        let inline sum upperBound exclusion fn =
            let mutable partialSum = 0.
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
            let x = Vector3(-dx, 0., dx)
            let numerator = (sum n j 
                (fun k -> product n (k, j) (fun l -> 0. - x.[l]))) 

            let denominator = product n (j, -1) (fun k -> x.[j] - x.[k]);
            numerator / denominator
           
        let inline mapi fn v = 
            let struct(x, y, z) = v
            struct(fn 0 x, fn 1 y, fn 2 z)

        let L' (yVals: struct(float<'u> * float<'u> * float<'u>), dx: float<'f>) = 
            let inline sum v =
                let struct(x, y, z) = v
                x + y + z

            yVals 
            |> mapi (fun i y -> (y |> float |> FloatWithMeasure<'u/'f>) * l'(i, float dx)) 
            |> sum
          
        let approximateGradient (pos: Vector3<m>, dimension): float<N> =
            let currentPosition = match dimension with 
                                  | X -> pos.X
                                  | Y -> pos.Y
                                  | Z -> pos.Z
            
            let dx = 0.0001<m>
            let positionGradient = struct(currentPosition - dx, currentPosition, currentPosition + dx)

            let potentials = positionGradient |> mapi (fun _ p -> 
                let radius = match dimension with
                             | X -> Vector3(p, pos.Y, pos.Z)
                             | Y -> Vector3(pos.X, p, pos.Z)
                             | Z -> Vector3(pos.X, pos.Y, p)
                U(m, B(M, radius)))

            L'(potentials, dx)

        Vector3(approximateGradient(position, X), approximateGradient(position, Y), approximateGradient(position, Z))

    let rec runSimulation (pair: byref<struct (SimulatedObject * SimulatedObject)>, 
                           dt: float<s>, 
                           externalBField: inref<Vector3<T>>,
                           callback: System.Func<bool>) =
        let inline delta (initial: Vector3<'u/s>, acceleration: Vector3<'u/s^2>) = 
            (initial * dt) + (acceleration * (dt |> squared) * 0.5)
            
        // let inline angleBetween (a, b) = Acos(Dot(a, b) / (a.Length * b.Length))

        let struct(o1, o2) = pair
            
        // The net force applied on both objects is the same (Newton's third law).
        // We can calculate the force applied to o1 as a result of o2's magnetic field,
        // and apply the opposite of that force to o2.
        // Since the radius of both balls is not the same, however, the threshold force
        // will need to be calculated independently for both.

        // Arbitrary value that balances the density of points with processing time.
        // For a sphere with a radius of 0.01, this should provide us with about 10 points
        // from one point on the sphere to its opposite point.
        let pointGap = 0.<m> // o1.Radius * 2f / 5f

        let pointsLength = 1 //Round(o1.Radius * 2f / pointGap, System.MidpointRounding.AwayFromZero) |> int

        let matrixLength = pointsLength * pointsLength * pointsLength
        
        let magneticForce = 
            Array.init matrixLength (fun i -> 
                let x = i % pointsLength |> float
                let y = i / pointsLength % pointsLength |> float
                let z = i / (pointsLength * pointsLength) % pointsLength |> float

                let indexToPos i = -o1.Radius + pointGap * i
                let unitPosition = Vector3(indexToPos x, indexToPos y, indexToPos z) 
                if Length(unitPosition) <= 1.<m> then 
                    let p = unitPosition * float o1.Radius + o1.Position
                    magneticForce(p - o2.Position, o1.MagneticMoment / float matrixLength, o2.MagneticMoment)
                else 
                    Vector3.Zero)
            |> Array.sum

        let calculateBallDelta (magneticForce: Vector3<N>, o, otherObject, externalBField) =
            let dynamicViscosity = 1.002e-3<Pa * s>
            // Stokes' law
            let gamma = (6. * PI * o.Radius * dynamicViscosity)
            let forceDrag = gamma * o.Velocity
            let acceleration: Vector3<m/s^2> = (magneticForce + forceDrag) / o.Mass

            // This isn't fully correct
            // Things to add:
            // - If movement is sharper than at threshold angle, movement goes along 
            //   previous trajectory
            //   - We can draw a "line" and if the angle is too sharp we snap the trajectory to it 
            //     by a certain amount (the projection, probably)
            //   - We can also create "tunnel cylinders" to demarkate the torn medium and diretion, and use those
                
            let torque = torque(o.MagneticMoment, B(otherObject.MagneticMoment, o.Position - otherObject.Position) + externalBField)

            // EXPERIMENTAL EVALUATION OF THE DRAG ROTATION COEFFICIENT OF THE SPHERICAL PARTICLE
            (*let coefficientKinematicViscosity = 1.<m^2/s>
            let dragRotationCoefficient = (16. * PI * coefficientKinematicViscosity) / (Length(o.AngularVelocity) * (o.Radius |> squared))
            let angularDrag: Vector3<A T m^2> = o.AngularVelocity * FloatWithMeasure dragRotationCoefficient*)
            let angularDrag: Vector3<A T m^2> = o.AngularVelocity * FloatWithMeasure 0.05
            let momentOfInertia = ((2. / 5.) * o.Mass * (o.Radius |> squared))
            let angularAcceleration: Vector3<1/s^2> = (torque - angularDrag) / momentOfInertia


            let dtheta = delta(o.AngularVelocity, angularAcceleration)

            // TODO: write our own quaternion transform
            let transform = 
                if dtheta <> Vector3.Zero then
                    let silkVector = Silk.NET.Maths.Vector3D(o.MagneticMoment.X, o.MagneticMoment.Y, o.MagneticMoment.Z)
                    let silkDtheta = Silk.NET.Maths.Vector3D(FloatWithMeasure dtheta.X, FloatWithMeasure dtheta.Y, FloatWithMeasure dtheta.Z)
                    let silkTransformed = Silk.NET.Maths.Vector3D.Transform(
                        silkVector,
                        Silk.NET.Maths.Quaternion.CreateFromAxisAngle(Silk.NET.Maths.Vector3D.Normalize(silkDtheta), silkDtheta.Length))
                    assert(silkTransformed = silkTransformed)
                    Vector3(silkTransformed.X, silkTransformed.Y, silkTransformed.Z)
                else 
                    o.MagneticMoment

            { o with
                PreviousPosition = o.Position
                Position = o.Position + delta(o.Velocity, acceleration)
                Velocity = o.Velocity + acceleration * dt
                MagneticMoment = transform
                AngularVelocity = o.AngularVelocity + (angularAcceleration * dt)
            }

        pair <- struct(calculateBallDelta (magneticForce, o1, o2, externalBField), calculateBallDelta (-magneticForce, o2, o1, externalBField))
        
        if callback.Invoke() then runSimulation (&pair, dt, &externalBField, callback) else ()
