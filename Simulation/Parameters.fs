namespace Simulation

open Vectors
open FSharp.Data.UnitSystems.SI.UnitSymbols

module Parameters =
    let standardObjects () =
        let largeRadius = 0.005<m>;
        let smallRadius = largeRadius * (2. / 3.)
        struct (
            { Position = Vector3(largeRadius + smallRadius, 0.<m>, 0.<m>); Radius = largeRadius; Path = [] },
            { Position = Vector3.Zero;                                     Radius = smallRadius; Path = [] })

    let fieldStrength isExpanding = if isExpanding then 2.<T> else 0.5<T>
