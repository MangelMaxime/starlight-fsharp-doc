module FSharp.Oracle.Program

open System
open FSharp.Oracle.Schema
open FSharp.Oracle.Extractor
open Thoth.Json.System.Text.Json
open Thoth.Json.Core.Auto

[<EntryPoint>]
let main argv =
    let rec parseArgs outputBase dlls =
        function
        | "--output-base" :: value :: rest -> parseArgs value dlls rest
        | path :: rest -> parseArgs outputBase (path :: dlls) rest
        | [] -> outputBase, List.rev dlls |> List.toArray

    let outputBase, dllPaths = parseArgs "api" [] (argv |> Array.toList)

    match dllPaths with
    | [||] ->
        eprintfn "Usage: fsharp-docs-oracle [--output-base <base>] <path/to/Assembly.dll> [...]"
        eprintfn "Output: JSON IR written to stdout"
        1
    | _ ->
        let assemblies = dllPaths |> Array.map (extractAssembly outputBase) |> Array.toList

        let root =
            {
                Assemblies = assemblies
            }

        let json =
            root
            |> Encode.Auto.generateEncoder(losslessOption = true)
            |> Encode.toString 4

        Console.WriteLine(json)

        0
