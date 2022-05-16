namespace Simulation

open System.Numerics
open type System.Numerics.Vector3
open type System.MathF

module Calculate =
    let Mu = (4f * PI * 10e-7f)

    let B M r = (Mu / (4f * PI)) * (3f * (Dot(M, r) * r) / (r.Length() ** 5f) - (M / (r.Length() ** 3f)))

    let U m B = Dot(-m, B)

    let potentialField magneticMomentSmall magneticMomentBig sideLength stepping =
        
        Array3D.init sideLength sideLength sideLength (fun x y z -> 
            U magneticMomentSmall
            <| B magneticMomentBig (Vector3(float32 (sideLength - 1) / 2f * stepping) - Vector3(float32 x * stepping, float32 y * stepping, float32 z * stepping)))

    [<Struct>]
    type Dimension = X | Y | Z

    let forceVectorField potentials =
        let edgeIndex dimension index =
            index = 0 || index + 1 = match dimension with 
                                     | X -> Array3D.length1 potentials
                                     | Y -> Array3D.length2 potentials
                                     | Z -> Array3D.length3 potentials

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
            | X -> L' [| potentials.[x - 1, y, z]; potentials.[x, y, z]; potentials.[x + 1, y, z] |]
            | Y -> L' [| potentials.[x, y - 1, z]; potentials.[x, y, z]; potentials.[x, y + 1, z] |]
            | Z -> L' [| potentials.[x, y, z - 1]; potentials.[x, y, z]; potentials.[x, y, z + 1] |]

        potentials |> Array3D.mapi (fun x y z _ -> 
            if edgeIndex X x || edgeIndex Y y || edgeIndex Z z then 
                Vector3() 
            else 
                -Vector3(approximateGradient x y z X, approximateGradient x y z Y, approximateGradient x y z Z))