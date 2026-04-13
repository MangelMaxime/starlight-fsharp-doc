/// <summary>Global utility functions available at the root level.</summary>
module Helpers

/// <summary>Returns the version string of the library.</summary>
let version = "0.1.0"

/// <summary>Returns true if the given string is non-empty.</summary>
/// <example>
/// Check a non-empty string:
/// <code lang="fsharp">
/// isNonEmpty "hello" // true
/// </code>
/// </example>
/// <example>
/// Check an empty string:
/// <code lang="fsharp">
/// isNonEmpty "" // false
/// </code>
/// </example>
let isNonEmpty (s: string) = s.Length > 0

/// <summary>Returns the current UTC time as an ISO-8601 string.</summary>
/// <returns>A formatted date-time string, e.g. <c>"2025-01-01T00:00:00Z"</c>.</returns>
/// <example>
/// <code lang="fsharp">
/// let now = timestamp ()
/// printfn "Current time: %s" now
/// </code>
/// </example>
let timestamp () = System.DateTime.UtcNow.ToString("o")

/// <summary>Returns the current UTC time as a Unix epoch timestamp.</summary>
/// <returns>Seconds since 1970-01-01T00:00:00Z.</returns>
[<System.Obsolete("Use `timestamp ()` instead.")>]
let epoch () =
    System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()

/// <summary>Pads a string on the left to a given width.</summary>
[<System.Obsolete>]
let padLeft (width: int) (s: string) = s.PadLeft(width)

/// <summary>Legacy configuration format. Use ConfigV2 instead.</summary>
[<System.Obsolete("Use ConfigV2 instead.")>]
type Config =
    {
        Name: string
        Value: int
    }

/// <summary>Legacy helper module. Use HelpersV2 instead.</summary>
[<System.Obsolete("Use HelpersV2 instead.")>]
module LegacyHelpers =
    let oldValue = 42
