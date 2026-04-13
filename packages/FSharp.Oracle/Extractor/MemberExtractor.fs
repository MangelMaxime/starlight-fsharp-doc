namespace FSharp.Oracle

open FSharp.Compiler.Symbols
open FSharp.Oracle.Schema
open Oracle.XmlDoc
open Helpers
open SignatureRendering
open ParameterExtractor

module internal MemberExtractor =
    /// Builds the signature TextNode for the per-member DocEntry slot (no anchors, no indentation).
    /// This is the pre-computed Declaration stored on Member.
    let buildMemberDeclaration
        (kind: MemberKind)
        (name: string)
        (isStatic: bool)
        (isAbstract: bool)
        (parameters: Parameter list list)
        (returnType: TextNode)
        (constraintClause: TextNode)
        : TextNode
        =
        let prefixNodes =
            match kind with
            | MemberKind.Constructor -> [ TextNode.Keyword "new" ]
            | MemberKind.Operator ->
                // Operators are always static in F#
                [
                    TextNode.Keyword "static"
                    TextNode.Space
                    TextNode.Keyword "member"
                    TextNode.Space
                    TextNode.Text name
                ]
            | MemberKind.Property ->
                if isStatic then
                    [
                        TextNode.Keyword "static"
                        TextNode.Space
                        TextNode.Keyword "property"
                        TextNode.Space
                        TextNode.Text name
                    ]
                elif isAbstract then
                    [
                        TextNode.Keyword "abstract"
                        TextNode.Space
                        TextNode.Keyword "property"
                        TextNode.Space
                        TextNode.Text name
                    ]
                else
                    [
                        TextNode.Keyword "property"
                        TextNode.Space
                        TextNode.Text name
                    ]
            | MemberKind.Method ->
                if isStatic then
                    [
                        TextNode.Keyword "static"
                        TextNode.Space
                        TextNode.Keyword "member"
                        TextNode.Space
                        TextNode.Text name
                    ]
                elif isAbstract then
                    [
                        TextNode.Keyword "abstract"
                        TextNode.Space
                        TextNode.Keyword "member"
                        TextNode.Space
                        TextNode.Text name
                    ]
                else
                    [
                        TextNode.Keyword "member"
                        TextNode.Space
                        TextNode.Text name
                    ]

        let allParams = parameters |> List.collect id

        let typeNodes =
            if allParams.IsEmpty then
                [
                    returnType
                    constraintClause
                ]
            else
                [
                    for i, param in List.indexed allParams do
                        if i > 0 then
                            yield TextNode.Space
                            yield TextNode.Arrow
                            yield TextNode.Space

                        yield param.Declaration
                    yield TextNode.Space
                    yield TextNode.Arrow
                    yield TextNode.Space
                    yield returnType
                    yield constraintClause
                ]

        let withGetNodes =
            if kind = MemberKind.Property then
                [
                    TextNode.Space
                    TextNode.Keyword "with"
                    TextNode.Space
                    TextNode.Keyword "get"
                ]
            else
                []

        TextNode.Node
            [
                yield! prefixNodes
                yield TextNode.Space
                yield TextNode.Colon
                yield TextNode.Space
                yield! typeNodes
                yield! withGetNodes
            ]

    let extractMember
        (toUrl: string -> string)
        (docs: Map<string, string>)
        (mfv: FSharpMemberOrFunctionOrValue)
        : Member
        =
        let obsoleteInfo = obsoleteOf mfv
        let kind = memberKindOf mfv
        // Constructors are all named ".ctor" by FCS; use "new" as a canonical
        // display name. Multiple-constructor disambiguation happens in extractEntity.
        let name =
            if mfv.IsConstructor then
                "new"
            else
                mfv.DisplayName

        let parameters = curriedParams toUrl mfv
        let returnType = renderFSharpType toUrl false mfv.ReturnParameter.Type
        let isStatic = mfv.IsModuleValueOrMember && not mfv.IsInstanceMember
        let isAbstract = mfv.IsDispatchSlot

        let genericParams = renderGenericParams toUrl mfv.GenericParameters

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

                TextNode.Node(TextNode.Space :: inner)
            | Some _ -> TextNode.Node []

        {
            Kind = kind
            Name = name
            FullName = mfv.FullName
            Parameters = parameters
            ReturnType = returnType
            Declaration =
                buildMemberDeclaration
                    kind
                    name
                    isStatic
                    isAbstract
                    parameters
                    returnType
                    constraintClause
            GenericParameters = genericParams
            XmlDoc = xmlDocOf docs mfv.XmlDocSig
            IsStatic = isStatic
            IsAbstract = isAbstract
            ObsoleteInfo = obsoleteInfo
        }
