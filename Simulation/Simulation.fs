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
        Position: Vector3<m>;
        Mass: float<kg>;
        Radius: float<m>
    }

module Calculate =
    // The F# pow operator calls into the CRT pow function for `float`.
    // B is in the innermost loop of the simulation, so pow often shows up very hot in profiles.
    // Manually writing out the multiplications avoids this problem.
    let inline pow5 v = v * v * v * v * v
    let inline pow4 v = v * v * v * v
    let inline cubed v = v * v * v
    let inline squared v = v * v

    let standardObjects () =
        let radius = 0.005<m>;
        let densityOfIron = 7874.<kg/m^3>;
        let circleVolume (r): float<m^3> = FloatWithMeasure ((4./3.) * PI) * (r |> cubed) 
        struct (
            { Position = Vector3(0.0075<m>, 0.<m>, 0.<m>); Radius = radius;           Mass = circleVolume(radius) * densityOfIron },
            { Position = Vector3.Zero;                     Radius = radius * (2./3.); Mass = circleVolume(radius * (2./5.)) * densityOfIron })

    let Mu_0 = FloatWithMeasure<H/m> (4. * PI * 1e-7)

    let H (M: Vector3<A m^2>, r: Vector3<m>) = (1. / (4. * PI)) * (3. * (Dot(M, r) * r) / (Length(r) |> pow5) - (M / (Length(r) |> cubed)))

    let B (M: Vector3<A m^2>, r): Vector3<T> = Mu_0 * H(M, r)

    let inline U (m: Vector3<A m^2>, B: Vector3<T>) = Dot(m, B)
        
    let inline torque (m: Vector3<A m^2>, B: Vector3<T>) = Cross(m, B)

    let magneticForce (r: Vector3<m>, m: Vector3<A m^2>, M: Vector3<A m^2>): Vector3<N> = 
        let f = (Dot(m, r) * M) + (Dot(M, r) * m) + (Dot(m, M) * r) - ((5. * Dot(m, r) * Dot(M, r)) / (Length(r) |> squared)) * r
        (3. * Mu_0) / (4. * PI * (Length(r) |> pow5)) * f

    let rec runSimulation (pair: byref<struct (SimulatedObject * SimulatedObject)>, 
                           dt: float<s>, 
                           externalBField: inref<Vector3<T>>,
                           isExpanding: bool,
                           callback: System.Func<bool, bool>) =
        let struct(l, s) = pair
            
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

        let valExternalBField = externalBField
        let magneticMoment (o: SimulatedObject) =
            let volume = (4./3.) * PI * (o.Radius |> cubed)
            (valExternalBField / Mu_0) * volume / 3.
        
        let largeMagneticForce = 
            Array.init matrixLength (fun i -> 
                let x = i % pointsLength |> float
                let y = i / pointsLength % pointsLength |> float
                let z = i / (pointsLength * pointsLength) % pointsLength |> float

                let indexToPos i = -l.Radius + pointGap * i
                let unitPosition = Vector3(indexToPos x, indexToPos y, indexToPos z) 
                if Length(unitPosition) <= 1.<m> then 
                    let p = unitPosition * float l.Radius + l.Position
                    magneticForce(p - s.Position, magneticMoment(l) / float matrixLength, magneticMoment(s))
                else 
                    Vector3.Zero)
            |> Array.sum

        let diameter (o: SimulatedObject) = o.Radius * 2.

        let forwardThreshold = 0.012<N>
        let sigma = forwardThreshold / ((PI / 4.) * ((3.2e-3<m>) |> squared))

        let backwardsThreshold = 0.008<N>
        let lambda = backwardsThreshold / (3.2e-3<m>)

        let gamma = 0.004<N> / (3.2e-3<m> * 0.5e-3<m/s>)

        let tearingVelocityMagnitude (force: Vector3<N>, threshold: float<N>, diameter: float<m>) = 
            let mag = (Length(force) - threshold) / (gamma * diameter)
            if mag >= 0.<m/s> then
                // if (Abs(float force.X) < float threshold) then
                //     maybe we shouldn't go?
                mag
            else
                0.<m/s>

        // let lambdaDl: float<N> = (-3. * Mu_0 * Dot(magneticMoment(s), magneticMoment(l))) / (2. * PI * (0.0103787100813698<m> |> pow4))
        // let Dmin : float<m> = ((3. * Mu_0 * Dot(magneticMoment(s), magneticMoment(l)))/(2. * PI * lambda * diameter(l))) |> sqrt |> sqrt
        // let Dmax : float<m> = ((3. * Mu_0 * Dot(magneticMoment(s), magneticMoment(l)))/(4. * PI * sigma * (diameter(l) |> squared))) |> sqrt |> sqrt

        let largeVelocity = 

            let largeThreshold = 
                if isExpanding then 
                    sigma * (diameter(l) |> squared)
                else
                    lambda * diameter(l)
            Normalize(largeMagneticForce) * tearingVelocityMagnitude (largeMagneticForce, largeThreshold, diameter(l)) 


        let smallVelocity = 
            let smallMagneticForce = -largeMagneticForce
            let smallThreshold = lambda * diameter(s)
            Normalize(smallMagneticForce) * tearingVelocityMagnitude (smallMagneticForce, smallThreshold, diameter(s))

        pair <- struct( 
            { l with Position = l.Position + largeVelocity * dt }, 
            { s with Position = s.Position + smallVelocity * dt })

       // If we are currently expanding, we should start contracting when the large ball can no longer overcome the threshold.
       // If we are currently contracting, we should start expanding when the large ball begins to overcome its threshold.
        let isExpanding = Length(largeVelocity) <> 0.<m/s>

        if callback.Invoke(isExpanding) then runSimulation (&pair, dt, &externalBField, isExpanding, callback) else ()
