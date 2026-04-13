namespace FSharp.Oracle

open FSharp.Compiler.Symbols
open FSharp.Oracle.Schema
open Oracle.XmlDoc
open Helpers
open SignatureRendering
open ParameterExtractor

module internal ValueExtractor =
    let extractFunction
        (toUrl: string -> string)
        (docs: Map<string, string>)
        (mfv: FSharpMemberOrFunctionOrValue)
        : Function
        =
        let obsoleteInfo = obsoleteOf mfv
        let parameters = curriedParams toUrl mfv
        let allParams = parameters |> List.collect id
        let returnTypeNode = renderFSharpType toUrl false mfv.ReturnParameter.Type

        // The colon column is determined by the longest of: function name, parameter names.
        // Both the `val name :` line and parameter lines align their colons to this column.
        let maxNameLength =
            let maxParamNameLength =
                if allParams.IsEmpty then
                    0
                else
                    allParams |> List.map (fun p -> p.Name.Length) |> List.max

            (max mfv.DisplayName.Length maxParamNameLength) + 1 // name + space before colon

        // Compute AlignedDeclaration for each parameter now that maxNameLength is known.
        let alignedParameters =
            parameters
            |> List.map (fun group ->
                group
                |> List.map (fun p ->
                    { p with
                        AlignedDeclaration =
                            TextNode.Node
                                [
                                    TextNode.NewLine
                                    TextNode.Spaces 4
                                    TextNode.Text p.Name
                                    TextNode.Spaces(maxNameLength - p.Name.Length)
                                    TextNode.Colon
                                    TextNode.Space
                                    p.Type
                                ]
                    }
                )
            )

        let genericParams = renderGenericParams toUrl mfv.GenericParameters

        let isInline =
            (mfv.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline
             || mfv.InlineAnnotation = FSharpInlineAnnotation.AggressiveInline)
            && not mfv.IsActivePattern

        let constraintClause =
            match genericParams with
            | None -> TextNode.Node []
            | Some(TextNode.Node nodes) ->
                // Strip < > and the leading type var, keep only ` when ...`
                let inner =
                    nodes
                    |> List.skipWhile (
                        function
                        | TextNode.LessThan -> true
                        | _ -> false
                    )
                    |> List.skipWhile (
                        function
                        | TextNode.Tick -> true
                        | TextNode.Text _ -> true
                        | _ -> false
                    )
                    |> List.rev
                    |> List.skipWhile (
                        function
                        | TextNode.GreaterThan -> true
                        | _ -> false
                    )
                    |> List.rev
                    |> List.skipWhile (
                        function
                        | TextNode.Space -> true
                        | _ -> false
                    )

                TextNode.Node(TextNode.Space :: inner)
            | Some _ -> TextNode.Node []

        let valKeyword =
            if isInline then
                "val inline"
            elif mfv.IsMutable then
                "val mutable"
            else
                "val"

        let simpleDeclaration =
            TextNode.Node
                [
                    TextNode.Keyword valKeyword
                    TextNode.Space
                    TextNode.Text mfv.DisplayName
                    TextNode.Space
                    TextNode.Colon
                ]

        let alignedDeclaration =
            TextNode.Node
                [
                    TextNode.Keyword valKeyword
                    TextNode.Space
                    TextNode.Text mfv.DisplayName
                    TextNode.Spaces(max (maxNameLength - mfv.DisplayName.Length) 1)
                    TextNode.Colon
                ]

        let signatureNode = renderFSharpType toUrl true mfv.FullType
        let xmlDoc = xmlDocOf docs mfv.XmlDocSig

        {
            Name = mfv.DisplayName
            FullName = mfv.FullName
            Signature = signatureNode
            Parameters = alignedParameters
            Declaration = simpleDeclaration
            AlignedDeclaration = alignedDeclaration
            GenericParameters = genericParams
            ReturnType =
                if allParams.IsEmpty then
                    TextNode.Node
                        [
                            TextNode.Space
                            returnTypeNode
                            constraintClause
                        ]
                else
                    // The arrow aligns with the colons above:
                    // 4 (indent) + maxNameLength
                    TextNode.Node
                        [
                            TextNode.NewLine
                            TextNode.Spaces(4 + maxNameLength)
                            TextNode.Arrow
                            TextNode.Space
                            returnTypeNode
                            constraintClause
                        ]
            XmlDoc = xmlDoc
            ObsoleteInfo = obsoleteInfo
        }

    let extractValue
        (toUrl: string -> string)
        (docs: Map<string, string>)
        (mfv: FSharpMemberOrFunctionOrValue)
        : Value
        =
        let obsoleteInfo = obsoleteOf mfv
        let typeNode = renderFSharpType toUrl true mfv.FullType
        let genericParams = renderGenericParams toUrl mfv.GenericParameters

        let isInline =
            (mfv.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline
             || mfv.InlineAnnotation = FSharpInlineAnnotation.AggressiveInline)
            && not mfv.IsActivePattern

        let constraintClause =
            match genericParams with
            | None -> TextNode.Node []
            | Some(TextNode.Node nodes) ->
                let inner =
                    nodes
                    |> List.skipWhile (
                        function
                        | TextNode.LessThan -> true
                        | _ -> false
                    )
                    |> List.skipWhile (
                        function
                        | TextNode.Tick -> true
                        | TextNode.Text _ -> true
                        | _ -> false
                    )
                    |> List.rev
                    |> List.skipWhile (
                        function
                        | TextNode.GreaterThan -> true
                        | _ -> false
                    )
                    |> List.rev
                    |> List.skipWhile (
                        function
                        | TextNode.Space -> true
                        | _ -> false
                    )

                TextNode.Node(TextNode.Space :: inner)
            | Some _ -> TextNode.Node []

        let valKeyword =
            if isInline then
                "val inline"
            elif mfv.IsMutable then
                "val mutable"
            else
                "val"

        {
            Name = mfv.DisplayName
            FullName = mfv.FullName
            Signature = typeNode
            Declaration =
                TextNode.Node
                    [
                        TextNode.Keyword valKeyword
                        TextNode.Space
                        TextNode.Text mfv.DisplayName
                        TextNode.Space
                        TextNode.Colon
                        TextNode.Space
                        typeNode
                        constraintClause
                    ]
            GenericParameters = genericParams
            XmlDoc = xmlDocOf docs mfv.XmlDocSig
            ObsoleteInfo = obsoleteInfo
        }
