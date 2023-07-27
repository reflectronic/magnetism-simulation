module Simulation.Vectors
open System
open NonStructuralComparison
open Microsoft.FSharp.Core.LanguagePrimitives

[<Struct>]
[<Diagnostics.DebuggerDisplay("(X={X}, Y={Y}, Z={Z})")>]
type Vector3<[<Measure>] 'T> =
    val X: float<'T>;
    val Y: float<'T>;
    val Z: float<'T>;

    new(x, y, z) = { X = x; Y = y; Z = z }

    static member (+) (left: Vector3<'T>, right: Vector3<'T>) =
        Vector3(left.X + right.X, left.Y + right.Y, left.Z + right.Z);

    static member (-) (left: Vector3<'T>, right: Vector3<'T>) =
        Vector3(left.X - right.X, left.Y - right.Y, left.Z - right.Z);

    static member (*) (vector: Vector3<'T>, scalar: float<_>) =
        Vector3(vector.X * scalar, vector.Y * scalar, vector.Z * scalar);

    static member (*) (scalar: float<_>, vector: Vector3<'T>) =
        Vector3(vector.X * scalar, vector.Y * scalar, vector.Z * scalar);

    static member (/) (vector: Vector3<'T>, scalar: float<_>) =
        Vector3(vector.X / scalar, vector.Y / scalar, vector.Z / scalar);

    static member op_Equality (left: Vector3<'T>, right: Vector3<'T>) =
        left.X = right.X && left.Y = right.Y && left.Z = right.Z

    static member op_Inequality (left: Vector3<'T>, right: Vector3<'T>) =
        not (left = right)

    static member (~-) (vector: Vector3<'T>) =
        Vector3(-vector.X, -vector.Y, -vector.Z)

    static member Zero = 
        Unchecked.defaultof<Vector3<'T>>

    member this.Item
        with get(i: int) = match i with
                           | 0 -> this.X
                           | 1 -> this.Y
                           | 2 -> this.Z
                           | _ -> invalidArg (nameof i) "Invalid index"

    override this.ToString() = 
        $"(X = {this.X}, Y = {this.Y}, Z = {this.Z})"


let NumericsVector (vector3: Vector3<_>) = 
    Numerics.Vector3(float32 vector3.X, float32 vector3.Y, float32 vector3.Z)

let SimulationVector (vector3: Numerics.Vector3) = 
    let convert = float >> FloatWithMeasure
    Vector3<'a>(convert vector3.X, convert vector3.Y, convert vector3.Z)
 
let Dot(left: Vector3<'L>, right: Vector3<'R>) =
        (left.X * right.X) + (left.Y * right.Y) + (left.Z * right.Z);

let Cross(left: Vector3<'L>, right: Vector3<'R>) =
        Vector3((left.Y * right.Z) - (left.Z * right.Y),
                (left.Z * right.X) - (left.X * right.Z),
                (left.X * right.Y) - (left.Y * right.X))

let Length(vector: Vector3<'T>) =
    Dot(vector, vector) |> sqrt

let Normalize(vector: Vector3<'T>) = 
    if vector = Vector3.Zero 
        then Vector3.Zero 
    else
        vector / Length(vector)
