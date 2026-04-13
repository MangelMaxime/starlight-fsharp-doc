/// <summary>String and text-processing utilities.</summary>
module Reference.Text

// ---------------------------------------------------------------------------
// Sub-module: word-level operations
// ---------------------------------------------------------------------------

/// <summary>Word-level string operations.</summary>
module Words =

    /// <summary>Capitalises the first character of a string.</summary>
    /// <param name="s">The input string.</param>
    /// <returns>
    /// A copy of <paramref name="s"/> with its first character converted to
    /// upper-case. Returns the empty string unchanged.
    /// </returns>
    /// <example>
    /// <code>
    /// Words.capitalize "hello"  // "Hello"
    /// Words.capitalize ""       // ""
    /// </code>
    /// </example>
    let capitalize (s: string) : string =
        if s.Length = 0 then
            s
        else
            System.Char.ToUpper(s.[0]).ToString() + s.[1..]

    /// <summary>Checks whether a string reads the same forwards and backwards.</summary>
    /// <param name="s">The input string (case-insensitive).</param>
    /// <example>
    /// <code>
    /// Words.isPalindrome "racecar"  // true
    /// Words.isPalindrome "hello"    // false
    /// </code>
    /// </example>
    let isPalindrome (s: string) : bool =
        let chars = s.ToLowerInvariant().ToCharArray()
        chars = Array.rev chars

// ---------------------------------------------------------------------------
// Sub-module: line-level operations
// ---------------------------------------------------------------------------

/// <summary>Line-level string operations.</summary>
module Lines =

    /// <summary>Splits text into individual lines, stripping empty lines.</summary>
    /// <param name="text">A multi-line input string.</param>
    /// <returns>An array of non-empty lines.</returns>
    let split (text: string) : string array =
        text.Split(
            [|
                '\n'
                '\r'
            |],
            System.StringSplitOptions.RemoveEmptyEntries
        )

    /// <summary>Joins an array of lines with a newline separator.</summary>
    /// <param name="lines">The lines to join.</param>
    /// <returns>A single string with lines separated by <c>\n</c>.</returns>
    let join (lines: string array) : string = System.String.Join("\n", lines)

    /// <summary>Returns the number of lines in a string.</summary>
    /// <param name="text">A multi-line input string.</param>
    let count (text: string) : int = (split text).Length
