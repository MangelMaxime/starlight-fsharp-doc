/// <summary>Geometric shape primitives used as an extractor test fixture.</summary>
module Reference.Shapes

open System

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// <summary>A 2D geometric shape.</summary>
/// <remarks>
/// Each case carries the minimum data needed to compute area and perimeter.
/// </remarks>
type Shape =
    /// <summary>A circle defined by its radius.</summary>
    | Circle of radius: float
    /// <summary>A rectangle defined by its width and height.</summary>
    | Rectangle of width: float * height: float
    /// <summary>An equilateral triangle defined by its side length.</summary>
    | Triangle of side: float

/// <summary>The axis-aligned bounding box of a shape.</summary>
type BoundingBox =
    {
        /// <summary>Width of the bounding box.</summary>
        Width: float
        /// <summary>Height of the bounding box.</summary>
        Height: float
    }

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

/// <summary>Computes the area of a shape.</summary>
/// <param name="shape">The shape whose area is computed.</param>
/// <returns>Area as a <c>float</c>.</returns>
/// <example>
/// <code>
/// let a = area (Circle 1.0)   // Math.PI
/// let b = area (Rectangle (3.0, 4.0))  // 12.0
/// </code>
/// </example>
let area (shape: Shape) : float =
    match shape with
    | Circle r -> Math.PI * r * r
    | Rectangle(w, h) -> w * h
    | Triangle s -> (Math.Sqrt 3.0 / 4.0) * s * s

/// <summary>Returns the axis-aligned bounding box that encloses <paramref name="shape"/>.</summary>
/// <param name="shape">The shape to bound.</param>
/// <returns>A <see cref="BoundingBox"/> record.</returns>
let boundingBox (shape: Shape) : BoundingBox =
    match shape with
    | Circle r ->
        {
            Width = 2.0 * r
            Height = 2.0 * r
        }
    | Rectangle(w, h) ->
        {
            Width = w
            Height = h
        }
    | Triangle s ->
        {
            Width = s
            Height = Math.Sqrt 3.0 / 2.0 * s
        }

/// <summary>Scales a shape by a uniform factor.</summary>
/// <param name="factor">The scaling factor to apply.</param>
/// <param name="shape">The shape to scale.</param>
/// <returns>A new <see cref="Shape"/> scaled by <paramref name="factor"/>.</returns>
let scale (factor: float) (shape: Shape) : Shape =
    match shape with
    | Circle r -> Circle(r * factor)
    | Rectangle(w, h) -> Rectangle(w * factor, h * factor)
    | Triangle s -> Triangle(s * factor)

/// <summary>Checks whether a point lies inside a shape's bounding box.</summary>
/// <param name="x">The x coordinate of the point.</param>
/// <param name="y">The y coordinate of the point.</param>
/// <param name="shape">The shape whose bounding box is tested.</param>
/// <returns><c>true</c> if the point is inside the bounding box.</returns>
let containsPoint (x: float) (y: float) (shape: Shape) : bool =
    let bb = boundingBox shape
    x >= 0.0 && x <= bb.Width && y >= 0.0 && y <= bb.Height

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

/// <summary>The unit circle — a <see cref="Circle"/> with radius <c>1.0</c>.</summary>
let unitCircle = Circle 1.0
