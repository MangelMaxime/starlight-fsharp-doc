namespace Reference.Geometry

open System

// ---------------------------------------------------------------------------
// Types declared directly in the namespace
// ---------------------------------------------------------------------------

/// <summary>A distance measured in metres.</summary>
type Meters = float

/// <summary>A 2D point in Euclidean space.</summary>
type Point =
    {
        /// <summary>The horizontal coordinate.</summary>
        X: float
        /// <summary>The vertical coordinate.</summary>
        Y: float
    }

/// <summary>An axis-aligned 2D rectangle defined by its two corners.</summary>
type Rect =
    {
        /// <summary>Top-left corner of the rectangle.</summary>
        TopLeft: Point
        /// <summary>Bottom-right corner of the rectangle.</summary>
        BottomRight: Point
    }

/// <summary>A 2D vector with X and Y components.</summary>
type Vector =
    {
        /// <summary>Horizontal component.</summary>
        X: float
        /// <summary>Vertical component.</summary>
        Y: float
    }

    /// <summary>The Euclidean length (magnitude) of the vector.</summary>
    member this.Length = Math.Sqrt(this.X * this.X + this.Y * this.Y)

    /// <summary>Adds another vector to this one.</summary>
    /// <param name="other">The vector to add.</param>
    /// <returns>A new <see cref="Vector"/> that is the component-wise sum.</returns>
    member this.Add(other: Vector) =
        {
            X = this.X + other.X
            Y = this.Y + other.Y
        }

    /// <summary>Scales the vector by a scalar factor.</summary>
    /// <param name="factor">The scaling factor.</param>
    /// <returns>A new <see cref="Vector"/> with each component multiplied by <paramref name="factor"/>.</returns>
    member this.Scale(factor: float) =
        {
            X = this.X * factor
            Y = this.Y * factor
        }

    /// <summary>The zero vector — <c>(0, 0)</c>.</summary>
    static member Zero =
        {
            X = 0.0
            Y = 0.0
        }

// ---------------------------------------------------------------------------
// Module declared inside the namespace
// ---------------------------------------------------------------------------

/// <summary>Primitive operations on 2D points.</summary>
module Points =

    /// <summary>The origin — the point <c>(0, 0)</c>.</summary>
    let origin: Point =
        {
            X = 0.0
            Y = 0.0
        }

    /// <summary>Translates a point by a given offset.</summary>
    /// <param name="dx">Horizontal offset.</param>
    /// <param name="dy">Vertical offset.</param>
    /// <param name="p">The point to translate.</param>
    /// <returns>A new <see cref="Point"/> shifted by <c>(dx, dy)</c>.</returns>
    let translate (dx: float) (dy: float) (p: Point) : Point =
        {
            X = p.X + dx
            Y = p.Y + dy
        }

    /// <summary>Computes the Euclidean distance between two points.</summary>
    /// <param name="a">First point.</param>
    /// <param name="b">Second point.</param>
    /// <returns>Distance as a <c>float</c>.</returns>
    /// <example>
    /// <code>
    /// let d = Points.distance origin { X = 3.0; Y = 4.0 }  // 5.0
    /// </code>
    /// </example>
    let distance (a: Point) (b: Point) : float =
        let dx = b.X - a.X
        let dy = b.Y - a.Y
        Math.Sqrt(dx * dx + dy * dy)
