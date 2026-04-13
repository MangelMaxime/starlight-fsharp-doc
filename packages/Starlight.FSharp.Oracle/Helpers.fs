module Starlight.FSharp.Helpers

open Fable.Core
open Fable.Core.JsInterop
open Node.Api

/// Result type for file operations
type FileOperationResult<'T> =
    | Ok of 'T
    | Error of string

/// Wraps fs.writeFileSync with error handling
let writeSync (path: string) (content: string) : FileOperationResult<unit> =
    try
        fs.writeFileSync (path, content, box "utf-8")
        Ok()
    with ex ->
        Error $"Failed to write file '{path}': {ex.Message}"

/// Wraps fs.mkdirSync with error handling
let mkdirSync (path: string) : FileOperationResult<unit> =
    try
        fs?mkdirSync (
            path,
            {|
                recursive = true
            |}
        )
        |> ignore

        Ok()
    with ex ->
        Error $"Failed to create directory '{path}': {ex.Message}"

/// Collects all errors from a list of results
let collectErrors (results: FileOperationResult<unit> list) : string list =
    results
    |> List.choose (
        function
        | Error msg -> Some msg
        | Ok _ -> None
    )
