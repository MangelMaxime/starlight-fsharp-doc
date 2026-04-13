namespace FSharp.Oracle

open FSharp.Compiler.Symbols
open FSharp.Oracle.Schema
open Helpers

module internal SignatureRendering =
    let rec renderFSharpType
        (toUrl: string -> string)
        (isTopLevel: bool)
        (typ: FSharpType)
        : TextNode
        =
        if typ.IsGenericParameter then
            let gp = typ.GenericParameter

            TextNode.Node
                [
                    if isSrtp gp then
                        TextNode.Text "^"
                    else
                        TextNode.Tick
                    TextNode.Text gp.DisplayName
                ]
        elif typ.IsFunctionType then
            let parts =
                [
                    for i in 0 .. typ.GenericArguments.Count - 1 do
                        let arg = typ.GenericArguments.[i]

                        if i > 0 then
                            TextNode.Space
                            TextNode.Arrow
                            TextNode.Space

                        renderFSharpType toUrl false arg
                ]

            if isTopLevel then
                TextNode.Node parts
            else
                TextNode.Node
                    [
                        TextNode.LeftParen
                        yield! parts
                        TextNode.RightParen
                    ]
        elif typ.IsTupleType then
            TextNode.Node
                [
                    for i in 0 .. typ.GenericArguments.Count - 1 do
                        let arg = typ.GenericArguments.[i]

                        if i > 0 then
                            TextNode.Space
                            TextNode.Star
                            TextNode.Space

                        renderFSharpType toUrl false arg
                ]
        elif typ.IsAnonRecordType then
            let details = typ.AnonRecordTypeDetails
            let names = details.SortedFieldNames
            let types = typ.GenericArguments |> Seq.toArray

            let fields =
                names
                |> Array.mapi (fun i name ->
                    let fieldType = types.[i]

                    TextNode.Node
                        [
                            TextNode.Text name
                            TextNode.Space
                            TextNode.Colon
                            TextNode.Space
                            renderFSharpType toUrl false fieldType
                        ]
                )
                |> Array.toList

            TextNode.Node
                [
                    yield TextNode.Text "{|"
                    yield TextNode.Space
                    for i, field in List.indexed fields do
                        if i > 0 then
                            yield TextNode.Text ";"
                            yield TextNode.Space

                        yield field
                    yield TextNode.Space
                    yield TextNode.Text "|}"
                ]
        elif typ.HasTypeDefinition && typ.GenericArguments.Count = 0 then
            let td = typ.TypeDefinition

            match tryGetFullName td with
            | Some fullName -> TextNode.TypeRef(td.DisplayName, fullName, toUrl fullName)
            | None -> TextNode.Text td.DisplayName
        elif
            typ.HasTypeDefinition
            && typ.TypeDefinition.IsArrayType
            && typ.GenericArguments.Count = 1
        then
            let arg = renderFSharpType toUrl false typ.GenericArguments.[0]

            TextNode.Node
                [
                    arg
                    TextNode.Text "[]"
                ]
        elif typ.HasTypeDefinition && typ.GenericArguments.Count = 1 then
            let td = typ.TypeDefinition
            let arg = renderFSharpType toUrl false typ.GenericArguments.[0]

            let isPostfix =
                Set.contains td.DisplayName postfixTypeDisplayNames
                || match tryGetFullName td with
                   | Some fullName -> Set.contains fullName postfixTypeNames
                   | None -> false

            if isPostfix then
                let head =
                    match tryGetFullName td with
                    | Some fullName -> TextNode.TypeRef(td.DisplayName, fullName, toUrl fullName)
                    | None -> TextNode.Text td.DisplayName

                TextNode.Node
                    [
                        arg
                        TextNode.Space
                        head
                    ]
            else
                let args =
                    [
                        for i in 0 .. typ.GenericArguments.Count - 1 do
                            let a = typ.GenericArguments.[i]

                            if i > 0 then
                                TextNode.Comma
                                TextNode.Space

                            renderFSharpType toUrl false a
                    ]

                let head =
                    match tryGetFullName td with
                    | Some fullName -> TextNode.TypeRef(td.DisplayName, fullName, toUrl fullName)
                    | None -> TextNode.Text td.DisplayName

                TextNode.Node
                    [
                        head
                        TextNode.LessThan
                        yield! args
                        TextNode.GreaterThan
                    ]
        elif typ.HasTypeDefinition then
            let td = typ.TypeDefinition

            let args =
                [
                    for i in 0 .. typ.GenericArguments.Count - 1 do
                        let arg = typ.GenericArguments.[i]

                        if i > 0 then
                            TextNode.Comma
                            TextNode.Space

                        renderFSharpType toUrl false arg
                ]

            let head =
                match tryGetFullName td with
                | Some fullName -> TextNode.TypeRef(td.DisplayName, fullName, toUrl fullName)
                | None -> TextNode.Text td.DisplayName

            TextNode.Node
                [
                    head
                    TextNode.LessThan
                    yield! args
                    TextNode.GreaterThan
                ]
        else
            TextNode.Text(typ.Format FSharpDisplayContext.Empty)

    /// Render a single generic parameter constraint as a TextNode list.
    let renderConstraint
        (toUrl: string -> string)
        (gp: FSharpGenericParameter)
        (c: FSharpGenericParameterConstraint)
        : TextNode list
        =
        if c.IsComparisonConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "comparison"
            ]
        elif c.IsEqualityConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "equality"
            ]
        elif c.IsUnmanagedConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "unmanaged"
            ]
        elif c.IsNonNullableValueTypeConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "struct"
            ]
        elif c.IsReferenceTypeConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "not null"
            ]
        elif c.IsNotSupportsNullConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "not null"
            ]
        elif c.IsSupportsNullConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "null"
            ]
        elif c.IsCoercesToConstraint then
            let ty = c.CoercesToTarget

            [
                TextNode.Text ":>"
                TextNode.Space
                renderFSharpType toUrl false ty
            ]
        elif c.IsMemberConstraint then
            // SRTP — render simplified member constraint
            let m = c.MemberConstraintData
            let argTypes = m.MemberArgumentTypes |> Seq.toList

            let argNodes =
                if List.isEmpty argTypes then
                    []
                else
                    [
                        TextNode.Space
                        for i, arg in List.indexed argTypes do
                            if i > 0 then
                                TextNode.Space
                                TextNode.Star
                                TextNode.Space

                            renderFSharpType toUrl false arg
                    ]

            let retNodes =
                let rt = m.MemberReturnType

                if isNull (box rt) then
                    []
                elif List.isEmpty argTypes then
                    // No args, just return type: `: ret`
                    [
                        TextNode.Space
                        renderFSharpType toUrl false rt
                    ]
                else
                    // Args + return type: `: args -> ret`
                    [
                        TextNode.Space
                        TextNode.Arrow
                        TextNode.Space
                        renderFSharpType toUrl false rt
                    ]

            [
                TextNode.Colon
                TextNode.Space
                TextNode.LeftParen
                if m.MemberIsStatic then
                    TextNode.Keyword "static member"
                else
                    TextNode.Keyword "member"
                TextNode.Space
                TextNode.Text m.MemberName
                if not (List.isEmpty argTypes) || not (isNull (box m.MemberReturnType)) then
                    TextNode.Space
                    TextNode.Colon
                    yield! argNodes
                    yield! retNodes
                TextNode.RightParen
            ]
        elif c.IsRequiresDefaultConstructorConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.LeftParen
                TextNode.Keyword "new"
                TextNode.Space
                TextNode.Colon
                TextNode.Space
                TextNode.Text "unit"
                TextNode.Space
                TextNode.Arrow
                TextNode.Space
                TextNode.Tick
                TextNode.Text gp.DisplayName
                TextNode.RightParen
            ]
        elif c.IsEnumConstraint then
            let target = c.EnumConstraintTarget

            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "enum"
                TextNode.LessThan
                renderFSharpType toUrl false target
                TextNode.GreaterThan
            ]
        elif c.IsDelegateConstraint then
            [
                TextNode.Colon
                TextNode.Space
                TextNode.Keyword "delegate"
            ]
        elif c.IsDefaultsToConstraint then
            let data = c.DefaultsToConstraintData

            [
                TextNode.Equal
                TextNode.Space
                renderFSharpType toUrl false data.DefaultsToTarget
            ]
        elif c.IsSimpleChoiceConstraint then
            c.SimpleChoices
            |> Seq.map (renderFSharpType toUrl false)
            |> Seq.toList
            |> List.mapi (fun i tn ->
                if i = 0 then
                    tn
                else
                    TextNode.Node
                        [
                            TextNode.Space
                            TextNode.Text "|"
                            TextNode.Space
                            tn
                        ]
            )
        else
            []

    /// Render the generic-parameter list for a function or member, e.g.
    /// `<'T when 'T : comparison>` or `<^T when ^T : (static member (+) : ^T * ^T -> ^T)>`.
    /// Returns `None` when there are no parameters.
    let renderGenericParams
        (toUrl: string -> string)
        (gps: FSharpGenericParameter seq)
        : TextNode option
        =
        let gps = gps |> Seq.toList

        if List.isEmpty gps then
            None
        else
            let nodes =
                [
                    TextNode.LessThan
                    for i, gp in List.indexed gps do
                        if i > 0 then
                            TextNode.Comma
                            TextNode.Space
                        // Use ^ only for SRTP (member constraints)
                        if isSrtp gp then
                            TextNode.Text "^"
                        else
                            TextNode.Tick

                        TextNode.Text gp.DisplayName

                        let constraints =
                            gp.Constraints |> Seq.toList |> List.map (renderConstraint toUrl gp)

                        if not (List.isEmpty constraints) then
                            TextNode.Space
                            TextNode.Keyword "when"
                            TextNode.Space

                            if isSrtp gp then
                                TextNode.Text "^"
                            else
                                TextNode.Tick

                            TextNode.Text gp.DisplayName

                            for i, cList in List.indexed constraints do
                                TextNode.Space

                                if i > 0 then
                                    TextNode.Keyword "and"
                                    TextNode.Space

                                    if isSrtp gp then
                                        TextNode.Text "^"
                                    else
                                        TextNode.Tick

                                    TextNode.Text gp.DisplayName
                                    TextNode.Space

                                for cNode in cList do
                                    cNode
                    TextNode.GreaterThan
                ]

            Some(TextNode.Node nodes)
