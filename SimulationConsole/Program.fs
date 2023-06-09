open Simulation
open Vectors

open System
open System.IO
open System.Linq
open System.Diagnostics
open System.Collections.Concurrent

open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Collections.ParallelSeq

open NonStructuralComparison

let fieldStrengths =
    Array2D.init 100 100 (fun x y -> 
        struct((float x * 0.01<T>) + 0.2<T>, (float y * 0.01<T>) + 0.2<T>)
    ) 
    |> Seq.cast<struct(float<T> * float<T>)>
    |> Seq.toArray

printf "Generated fields, starting simulation"

let stopwatch = Stopwatch.StartNew()

[<Struct>]
type Result = 
    | InitialExpansion
    | ContractionStart of smallPosition: Vector3<m>
    | ContractionEnd of smallPositionChange: Vector3<m> * largePosition: Vector3<m>
    | ExpansionEnd of float

let validResults = 
    Partitioner.Create(fieldStrengths, loadBalance = true).AsParallel()
    |> PSeq.filter (fun fieldStrengths -> (
        let struct(contractingStrength, expandingStrength) = fieldStrengths
        let ratio = (contractingStrength / expandingStrength)
        ratio > 0.249 && ratio < 0.251 && contractingStrength < expandingStrength && expandingStrength * 2. > contractingStrength
    )) 
    |> PSeq.map (fun fieldStrengths -> (
        let struct(contractingStrength, expandingStrength) = fieldStrengths

        let objects = Parameters.standardObjects()
        let startExpanding = true
        (fieldStrengths, Simulation.run(objects, 0.000001<s>, Vector3.Zero, startExpanding) struct(startExpanding, startExpanding, 0, InitialExpansion, Unchecked.defaultof<_>, 0) <| fun state -> (
            let struct (pair, shouldExpand, struct (wasExpanding, wasPreviouslyExpanding, phase, result, lastPair, count)) = state
            let struct (l, s) = pair
            let struct (lastL, lastS) = lastPair

            let checkpoint = if shouldExpand = wasExpanding then phase else phase + 1

            // Mechanical advantage = movement of large sphere during expansion / movement of small sphere during contraction
            let result = 
                if shouldExpand = wasExpanding then result 
                else
                    match struct (checkpoint, result) with
                    | (1, InitialExpansion) -> ContractionStart(s.Position)
                    | (2, ContractionStart(smallStart)) -> ContractionEnd(s.Position - smallStart, l.Position)
                    | (3, ContractionEnd(smallPositionChange, largeStart)) -> ExpansionEnd((Length(l.Position - largeStart))/ (Length(smallPositionChange)))
                    | (_, _) -> raise (UnreachableException())

            match result with
            | _ when checkpoint <> 0 && not wasExpanding && wasPreviouslyExpanding && lastL <> l -> EndSimulation(nan, count)
            | _ when l.Position = lastL.Position && s.Position = lastS.Position -> EndSimulation(nan, count)
            | ExpansionEnd(mechanicalEfficiency) -> EndSimulation((mechanicalEfficiency, count))
            | _ -> ContinueSimulation(Calculate.BFromPositions(pair, shouldExpand, if shouldExpand then expandingStrength else contractingStrength), struct(shouldExpand, wasExpanding, checkpoint, result, pair, count + 1))
        ))
    ))
    |> PSeq.filter (fun f -> (
        let (_, (mechanicalAdvantage, _)) = f
        mechanicalAdvantage <> 0.0 && System.Double.IsNaN(mechanicalAdvantage) |> not
    ))
    |> PSeq.withExecutionMode ParallelExecutionMode.ForceParallelism
    |> PSeq.withMergeOptions ParallelMergeOptions.FullyBuffered


let outputDirectory = Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory)
let outputPath = Path.Join(outputDirectory, "data.csv")

File.WriteAllText(outputPath, "Contracting field,Expanding field,Contracting to expanding field ratio,Mechanical efficiency,Simulation count\n")
for result in validResults do 
    let (strengths, (me, count)) = result
    let struct (contracting, expanding) = strengths
    printfn $"|{strengths,-50}|{contracting/expanding,22}|{me,22}|{count,10}"
    System.IO.File.AppendAllText(outputPath, $"%O{contracting},%O{expanding},%O{contracting/expanding},%O{me},%O{count}\n")

Media.SystemSounds.Beep.Play()
printfn "%A" stopwatch.Elapsed
