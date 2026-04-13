/// <summary>Object-oriented types used as an extractor test fixture.</summary>
module Reference.Objects

// ---------------------------------------------------------------------------
// Interface
// ---------------------------------------------------------------------------

/// <summary>Defines a contract for types that can describe themselves.</summary>
type IDescribable =
    /// <summary>Returns a human-readable description of this object.</summary>
    /// <returns>A plain-text description string.</returns>
    abstract member Describe: unit -> string

    /// <summary>The short label used to identify this object.</summary>
    abstract member Label: string

// ---------------------------------------------------------------------------
// Class
// ---------------------------------------------------------------------------

/// <summary>A named counter that tracks a running total.</summary>
type Counter(label: string) =

    let mutable count = 0

    /// <summary>Creates a counter with the label <c>"default"</c>.</summary>
    new() = Counter("default")

    /// <summary>The label identifying this counter.</summary>
    member _.Label = label

    /// <summary>The current value of the counter.</summary>
    member _.Count = count

    /// <summary>Increments the counter by one.</summary>
    member _.Increment() = count <- count + 1

    /// <summary>Increments the counter by a given amount.</summary>
    /// <param name="amount">The amount to add.</param>
    member _.IncrementBy(amount: int) = count <- count + amount

    /// <summary>Returns a string representation of the counter.</summary>
    /// <returns>A string of the form <c>"label: N"</c>.</returns>
    member this.Format() = $"{this.Label}: {count}"

    /// <summary>A counter pre-labelled <c>"global"</c>.</summary>
    static member Global = Counter("global")

// ---------------------------------------------------------------------------
// Operator overloads
// ---------------------------------------------------------------------------

/// <summary>A two-dimensional vector with operator overloads.</summary>
type Vector2D(x: float, y: float) =

    /// <summary>The x component.</summary>
    member _.X = x

    /// <summary>The y component.</summary>
    member _.Y = y

    /// <summary>Adds two vectors component-wise.</summary>
    /// <param name="a">Left operand.</param>
    /// <param name="b">Right operand.</param>
    static member (+)(a: Vector2D, b: Vector2D) = Vector2D(a.X + b.X, a.Y + b.Y)

    /// <summary>Subtracts two vectors component-wise.</summary>
    /// <param name="a">Left operand.</param>
    /// <param name="b">Right operand.</param>
    static member (-)(a: Vector2D, b: Vector2D) = Vector2D(a.X - b.X, a.Y - b.Y)

    /// <summary>Scales a vector by a scalar.</summary>
    /// <param name="v">The vector to scale.</param>
    /// <param name="s">The scalar multiplier.</param>
    static member (*)(v: Vector2D, s: float) = Vector2D(v.X * s, v.Y * s)
