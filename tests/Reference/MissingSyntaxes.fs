/// <summary>Demonstrates F# language features not covered by other test modules.</summary>
module Reference.MissingSyntaxes

open System

// ---------------------------------------------------------------------------
// Units of measure
// ---------------------------------------------------------------------------

/// <summary>A unit of length.</summary>
[<Measure>]
type m

/// <summary>A unit of time.</summary>
[<Measure>]
type s

/// <summary>A unit of velocity (metres per second).</summary>
[<Measure>]
type m_s = m / s

/// <summary>Converts a raw float to a metres value.</summary>
let metres (x: float) : float<m> = LanguagePrimitives.FloatWithMeasure x

/// <summary>Converts a raw float to a seconds value.</summary>
let seconds (x: float) : float<s> = LanguagePrimitives.FloatWithMeasure x

/// <summary>Computes velocity from distance and time.</summary>
let velocity (dist: float<m>) (time: float<s>) : float<m_s> =
    dist / time

// ---------------------------------------------------------------------------
// Struct types
// ---------------------------------------------------------------------------

/// <summary>A lightweight 2D point stored on the stack.</summary>
[<Struct>]
type StructPoint =
    {
        X: float
        Y: float
    }

/// <summary>A lightweight discriminated union stored on the stack.</summary>
[<Struct>]
type StructOption<'T> =
    | StructSome of 'T
    | StructNone

// ---------------------------------------------------------------------------
// Exception types
// ---------------------------------------------------------------------------

/// <summary>Raised when a parsing operation fails.</summary>
exception ParseError of message: string * position: int

/// <summary>Raised when a required resource is not found.</summary>
exception ResourceNotFound of resourceName: string

// ---------------------------------------------------------------------------
// Delegate types
// ---------------------------------------------------------------------------

/// <summary>A callback that transforms an integer.</summary>
type IntTransformer = delegate of int -> int

/// <summary>A callback that combines two values.</summary>
type Combiner<'T> = delegate of 'T * 'T -> 'T

// ---------------------------------------------------------------------------
// Type extensions
// ---------------------------------------------------------------------------

type StructPoint with

    /// <summary>Computes the Euclidean distance from the origin.</summary>
    member this.DistanceFromOrigin =
        Math.Sqrt(this.X * this.X + this.Y * this.Y)

    /// <summary>Translates the point by a given offset.</summary>
    member this.Translate(dx: float, dy: float) =
        { X = this.X + dx; Y = this.Y + dy }

// ---------------------------------------------------------------------------
// Cross-reference demo (<see cref>)
// ---------------------------------------------------------------------------

/// <summary>
/// Uses <see cref="T:Reference.MissingSyntaxes.StructPoint"/> and
/// <see cref="M:Reference.MissingSyntaxes.velocity"/> to demonstrate
/// cross-linking in XML documentation.
/// </summary>
let demoCrossRef (p: StructPoint) (v: float<m_s>) =
    printfn $"Point at ({p.X}, {p.Y}) moving at {v}"
