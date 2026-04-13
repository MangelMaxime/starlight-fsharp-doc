/// <summary>Active patterns used to test pattern matching extraction.</summary>
module Reference.ActivePatterns

open System

// ---------------------------------------------------------------------------
// Single-case active patterns
// ---------------------------------------------------------------------------

/// <summary>Parses a string as an integer using a single-case active pattern.</summary>
/// <param name="str">The string to parse.</param>
/// <returns>The integer if parsing succeeds.</returns>
let (|Integer|) (str: string) : int option =
    match Int32.TryParse(str) with
    | true, n -> Some n
    | _ -> None

/// <summary>Normalizes whitespace in a string.</summary>
/// <param name="str">The input string.</param>
/// <returns>The string with normalized whitespace.</returns>
let (|NormalizeSpaces|) (str: string) : string =
    str.Split(
        [|
            ' '
            '\t'
        |],
        StringSplitOptions.RemoveEmptyEntries
    )
    |> String.concat " "

// ---------------------------------------------------------------------------
// Multi-case active patterns
// ---------------------------------------------------------------------------

/// <summary>Recognizes different shapes of numeric values.</summary>
/// <param name="n">The number to classify.</param>
/// <returns>The shape classification.</returns>
let (|Positive|Negative|Zero|) (n: float) : Choice<unit, unit, unit> =
    if n > 0.0 then
        Positive
    elif n < 0.0 then
        Negative
    else
        Zero

/// <summary>Classifies a day of the week.</summary>
/// <param name="date">The date to classify.</param>
/// <returns>Weekday or Weekend classification.</returns>
let (|Weekday|Weekend|) (date: DateTime) : Choice<unit, unit> =
    match date.DayOfWeek with
    | DayOfWeek.Saturday
    | DayOfWeek.Sunday -> Weekend
    | _ -> Weekday

/// <summary>Analyzes string length.</summary>
/// <param name="str">The string to analyze.</param>
/// <returns>Classification by length.</returns>
let (|Empty|Short|Medium|Long|) (str: string) : Choice<unit, unit, unit, unit> =
    match str.Length with
    | 0 -> Empty
    | n when n < 10 -> Short
    | n when n < 100 -> Medium
    | _ -> Long

/// <summary>Classifies an option value.</summary>
/// <typeparam name="T">The option's value type.</typeparam>
/// <param name="opt">The option to classify.</param>
/// <returns>Some or None classification.</returns>
let (|IsSome|IsNone|) (opt: 'T option) : Choice<'T, unit> =
    match opt with
    | Some v -> IsSome v
    | None -> IsNone

/// <summary>Decomposes a result into success or failure.</summary>
/// <typeparam name="T">The success type.</typeparam>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="result">The result to decompose.</param>
/// <returns>Success or Failure classification.</returns>
let (|Success|Failure|) (result: Result<'T, 'E>) : Choice<'T, 'E> =
    match result with
    | Ok v -> Success v
    | Error e -> Failure e

// ---------------------------------------------------------------------------
// Partial active patterns
// ---------------------------------------------------------------------------

/// <summary>Tries to parse a string as an even integer.</summary>
/// <param name="str">The string to parse.</param>
/// <returns>The even integer if successful.</returns>
let (|EvenInt|_|) (str: string) : int option =
    match Int32.TryParse(str) with
    | true, n when n % 2 = 0 -> Some n
    | _ -> None

/// <summary>Tries to parse a string as a prime number.</summary>
/// <param name="str">The string to parse.</param>
/// <returns>The prime number if successful.</returns>
let (|Prime|_|) (str: string) : int option =
    let isPrime n =
        if n < 2 then
            false
        else
            let max = int (Math.Sqrt(float n))
            seq { 2..max } |> Seq.exists (fun x -> n % x = 0) |> not

    match Int32.TryParse(str) with
    | true, n when isPrime n -> Some n
    | _ -> None

/// <summary>Recognizes palindrome strings.</summary>
/// <param name="str">The string to check.</param>
/// <returns>The palindrome if it is one.</returns>
let (|Palindrome|_|) (str: string) : string option =
    let normalized = str.ToLower().Replace(" ", "")
    let reversed = normalized |> Seq.rev |> String.Concat

    if normalized = reversed then
        Some str
    else
        None

/// <summary>Recognizes valid email addresses (simplified).</summary>
/// <param name="str">The string to validate.</param>
/// <returns>The email if valid.</returns>
let (|Email|_|) (str: string) : string option =
    if str.Contains("@") && str.Contains(".") then
        Some str
    else
        None

/// <summary>Recognizes valid URL strings (simplified).</summary>
/// <param name="str">The string to validate.</param>
/// <returns>The URL if valid.</returns>
let (|Url|_|) (str: string) : string option =
    if str.StartsWith("http://") || str.StartsWith("https://") then
        Some str
    else
        None

/// <summary>Tries to extract a key-value pair from a string.</summary>
/// <param name="str">The string like "key=value".</param>
/// <returns>The key and value if the format matches.</returns>
let (|KeyValue|_|) (str: string) : (string * string) option =
    match str.Split('=') with
    | [| key; value |] -> Some(key, value)
    | _ -> None

// ---------------------------------------------------------------------------
// Parameterized active patterns
// ---------------------------------------------------------------------------

/// <summary>Checks if a number is divisible by a given divisor.</summary>
/// <param name="divisor">The number to divide by.</param>
/// <param name="n">The number to check.</param>
/// <returns>The quotient if divisible.</returns>
let (|DivisibleBy|_|) (divisor: int) (n: int) : int option =
    if n % divisor = 0 then
        Some(n / divisor)
    else
        None

/// <summary>Checks if a string matches a regex pattern.</summary>
/// <param name="pattern">The regex pattern.</param>
/// <param name="str">The string to match.</param>
/// <returns>The matched value if successful.</returns>
let (|RegexMatch|_|) (pattern: string) (str: string) : string option =
    let regex = Text.RegularExpressions.Regex(pattern)
    let m = regex.Match(str)

    if m.Success then
        Some(m.Groups.[0].Value)
    else
        None

/// <summary>Checks if a string starts with a given prefix.</summary>
/// <param name="prefix">The prefix to check.</param>
/// <param name="str">The string to check.</param>
/// <returns>The remainder if it starts with the prefix.</returns>
let (|StartsWith|_|) (prefix: string) (str: string) : string option =
    if str.StartsWith(prefix) then
        Some(str.Substring(prefix.Length))
    else
        None

/// <summary>Checks if a string ends with a given suffix.</summary>
/// <param name="suffix">The suffix to check.</param>
/// <param name="str">The string to check.</param>
/// <returns>The prefix if it ends with the suffix.</returns>
let (|EndsWith|_|) (suffix: string) (str: string) : string option =
    if str.EndsWith(suffix) then
        Some(str.Substring(0, str.Length - suffix.Length))
    else
        None

/// <summary>Checks if a number is within a range.</summary>
/// <param name="min">The minimum value (inclusive).</param>
/// <param name="max">The maximum value (inclusive).</param>
/// <param name="n">The number to check.</param>
/// <returns>The number if within range.</returns>
let (|InRange|_|) (min: int, max: int) (n: int) : int option =
    if n >= min && n <= max then
        Some n
    else
        None

/// <summary>Checks if a list contains exactly N elements.</summary>
/// <param name="n">The expected count.</param>
/// <param name="lst">The list to check.</param>
/// <returns>The list if it has exactly N elements.</returns>
let (|Exactly|_|) (n: int) (lst: 'T list) : 'T list option =
    if lst.Length = n then
        Some lst
    else
        None

// ---------------------------------------------------------------------------
// Combined active patterns
// ---------------------------------------------------------------------------

/// <summary>Recognizes different types of numeric strings.</summary>
/// <param name="str">The string to classify.</param>
/// <returns>Integer, Float, or NotNumeric classification.</returns>
let (|IntegerString|FloatString|NotNumeric|) (str: string) : Choice<int, float, unit> =
    match Int32.TryParse(str) with
    | true, n -> IntegerString n
    | _ ->
        match Double.TryParse(str) with
        | true, f -> FloatString f
        | _ -> NotNumeric

/// <summary>Decomposes a tree structure into cases.</summary>
/// <typeparam name="T">The tree element type.</typeparam>
/// <param name="tree">The tree to decompose.</param>
/// <returns>Single, Pair, or Many classification.</returns>
type Tree<'T> =
    | Empty
    | Leaf of 'T
    | Node of Tree<'T> * Tree<'T>

let (|Single|Pair|Many|) (tree: Tree<'T>) : Choice<'T, 'T * 'T, unit> =
    match tree with
    | Leaf v -> Single v
    | Node(Leaf a, Leaf b) -> Pair(a, b)
    | _ -> Many

/// <summary>Analyzes collection emptiness and size.</summary>
/// <param name="seq">The sequence to analyze.</param>
/// <returns>Empty, Singleton, or Multiple classification.</returns>
let (|EmptySeq|Singleton|Multiple|) (seq: seq<'T>) : Choice<unit, 'T, int> =
    let enumerator = seq.GetEnumerator()

    if not (enumerator.MoveNext()) then
        EmptySeq
    else
        let first = enumerator.Current

        if not (enumerator.MoveNext()) then
            Singleton first
        else
            let mutable count = 2

            while enumerator.MoveNext() do
                count <- count + 1

            Multiple count

// ---------------------------------------------------------------------------
// Active recognizers with side effects
// ---------------------------------------------------------------------------

/// <summary>Logs and returns the value (for debugging patterns).</summary>
/// <param name="label">The label to log.</param>
/// <param name="value">The value to return.</param>
/// <returns>The value unchanged.</returns>
let (|Log|) (label: string) (value: 'T) : 'T =
    printfn $"%s{label}: %A{value}"
    value

/// <summary>Times the execution of a function and returns the result with elapsed time.</summary>
/// <param name="f">The function to time.</param>
/// <returns>The result and elapsed milliseconds.</returns>
let (|Timed|) (f: unit -> 'T) : 'T * float =
    let sw = Diagnostics.Stopwatch.StartNew()
    let result = f ()
    sw.Stop()
    result, sw.Elapsed.TotalMilliseconds


