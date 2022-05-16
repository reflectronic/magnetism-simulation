namespace Simulation

open System.Numerics
open type System.Numerics.Vector3
open type System.MathF

module Calculate =
    let Mu = (4f * PI * 10e-7f)

    let B M r = (Mu / (4f * PI)) * (3f * (Dot(M, r) * r) / (r.Length() ** 5f) - (M / (r.Length() ** 3f)))

    let U m B = Dot(-m, B)

    let torque m B = Cross(m, B)

    [<Struct>]
    type Dimension = X | Y | Z

    let magneticFluxDensityField M sideLength stepping =
        Array3D.init sideLength sideLength sideLength (fun x y z -> 
            B M (Vector3(float32 (sideLength - 1) / 2f * stepping) - Vector3(float32 x * stepping, float32 y * stepping, float32 z * stepping)))

    let torqueVectorField m B = 
        B |> Array3D.map (fun B -> torque m B)
    
    let potentialField m B =
        B |> Array3D.map (fun B -> U m B)

    let forceVectorField U =
        let edgeIndex dimension index =
            index = 0 || index + 1 = match dimension with 
                                     | X -> Array3D.length1 U
                                     | Y -> Array3D.length2 U
                                     | Z -> Array3D.length3 U

        let product upperBound exclusions fn = 
            seq { 0..upperBound } 
            |> Seq.except exclusions 
            |> Seq.map (fun i -> fn i) 
            |> Seq.fold (fun partialProduct factor -> partialProduct * factor) 1

        let sum upperBound exclusions fn =
            seq { 0..upperBound }
            |> Seq.except exclusions
            |> Seq.sumBy (fun i -> fn i)

        let l' j n = 
            let numerator = (sum n (Seq.singleton j) 
                (fun k -> 
                    product n [| k; j |] (fun l -> (n / 2) - l))) 
            let denominator = product n (Seq.singleton j) (fun k -> j - k);
            float32 numerator / float32 denominator
        
        let L' yVals = 
            yVals 
            |> Array.mapi (fun i y -> y * float32 (l' i (yVals.Length - 1))) 
            |> Array.sum
                    
        let approximateGradient x y z dimension =
            match dimension with
            | X -> L' [| U.[x - 1, y, z]; U.[x, y, z]; U.[x + 1, y, z] |]
            | Y -> L' [| U.[x, y - 1, z]; U.[x, y, z]; U.[x, y + 1, z] |]
            | Z -> L' [| U.[x, y, z - 1]; U.[x, y, z]; U.[x, y, z + 1] |]

        U |> Array3D.mapi (fun x y z _ -> 
            if edgeIndex X x || edgeIndex Y y || edgeIndex Z z then 
                Vector3() 
            else 
                -Vector3(approximateGradient x y z X, approximateGradient x y z Y, approximateGradient x y z Z))

    let simulateForces sideLength stepping smallMagneticMoment bigMagneticMoment =
        magneticFluxDensityField bigMagneticMoment sideLength stepping
        |> potentialField smallMagneticMoment
        |> forceVectorField