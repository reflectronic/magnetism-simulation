open Simulation.Calculate
open Vectors

open type System.Math
open FSharp.Data.UnitSystems.SI.UnitSymbols

let mutable objects = Simulation.Calculate.standardObjects()

let run = 
    let externalField = Vector3<T>.Zero
    Simulation.Calculate.runSimulation(&objects, 0.000001<s>, &externalField, false, System.Func<bool, bool>(fun a -> (
        true
    )))
