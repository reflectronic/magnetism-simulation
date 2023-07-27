namespace Simulation    

open Vectors
open FSharp.Data.UnitSystems.SI.UnitSymbols
    
module Parameters =
    let standardObjects () =    
        let largeRadius = 0.005<m>;
        let smallRadius = largeRadius * (2. / 3.)

        let startingPos = Vector3(largeRadius + smallRadius * 1.5, 0.<m>, 0.<m>)
        let pt1 = { Radius = largeRadius; Position = startingPos }
        let pt2 = { Radius = largeRadius; Position = Vector3.Zero }

        struct (
            { Position = startingPos;  Radius = largeRadius; Path = [pt1; pt2] },
            { Position = Vector3.Zero; Radius = smallRadius; Path = [] })

    let fieldStrength isExpanding = if isExpanding then 0.8<T> else 0.25<T>
    