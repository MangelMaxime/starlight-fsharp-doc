namespace FSharp.Oracle

open FSharp.Compiler.Symbols
open FSharp.Oracle.Schema
open SignatureRendering

module internal ParameterExtractor =
    let extractParameter (toUrl: string -> string) (param: FSharpParameter) : Parameter =
        let name = param.DisplayName
        let typeNode = renderFSharpType toUrl false param.Type

        let declaration =
            TextNode.Node
                [
                    TextNode.TypeVar name
                    TextNode.Space
                    TextNode.Colon
                    TextNode.Space
                    typeNode
                ]

        {
            Name = name
            Type = typeNode
            Declaration = declaration
            AlignedDeclaration = declaration // overwritten by extractFunction once maxNameLength is known
        }

    let curriedParams (toUrl: string -> string) (mfv: FSharpMemberOrFunctionOrValue) =
        mfv.CurriedParameterGroups
        |> Seq.map (fun group -> group |> Seq.map (extractParameter toUrl) |> Seq.toList)
        |> Seq.toList
        // Drop unit-only groups for properties only — they are FCS artifacts on
        // no-arg getters (e.g. `member _.Zero`). For real functions and methods
        // (e.g. `let timestamp ()`) the unit group is explicit and must be kept.
        |> List.filter (fun group ->
            not (
                mfv.IsProperty
                && group.Length = 1
                && (
                    match group.[0].Type with
                    | TextNode.TypeRef(name, _, _) -> name = "unit"
                    | TextNode.Text "unit" -> true
                    | _ -> false
                )
            )
        )
