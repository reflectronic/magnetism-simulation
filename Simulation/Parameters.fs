namespace Simulation

open Vectors
open FSharp.Data.UnitSystems.SI.UnitSymbols

module Parameters =
    let standardObjects () =
        let radius = 0.005<m>;
        struct (
            { Position = Vector3(0.001<m>, 0.<m>, 0.<m>); Radius = radius; },
            { Position = Vector3.Zero;                     Radius = radius * (2./3.) })

    let fieldStrength isExpanding = if isExpanding then 1.<T> else 0.25<T>
