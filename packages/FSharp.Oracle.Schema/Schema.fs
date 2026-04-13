module FSharp.Oracle.Schema

// ---------------------------------------------------------------------------
// XML doc comment payloads
// ---------------------------------------------------------------------------

#nowarn 40

type XmlDocParam =
    {
        Name: string
        Doc: string
    }

type XmlDoc =
    {
        Summary: string option
        Remarks: string option
        Returns: string option
        Params: XmlDocParam list
        Examples: string list
    }

/// Whether an API is obsolete and, if so, whether a custom message was supplied.
[<RequireQualifiedAccess>]
type ObsoleteInfo =
    | Active
    | Deprecated
    | DeprecatedWithMessage of string

// ---------------------------------------------------------------------------
// Signature AST
// ---------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type TextNode =
    /// Plain identifier, e.g. "string", "int", "unit".
    | Text of string
    /// Named type with its full name for link generation, e.g. ("Option", "Microsoft.FSharp.Core.FSharpOption`1").
    /// The url field is the pre-computed page URL (slugified, with output base prefix).
    | TypeRef of name: string * fullName: string * url: string
    /// Generic type variable, e.g. "T" (the tick is implied — rendered as 'T).
    | TypeVar of string
    /// F# keyword, e.g. "val", "type".
    | Keyword of string
    | LeftBrace
    | RightBrace
    | Equal
    | Arrow
    | Tick
    | Space
    | Spaces of int
    | Comma
    | Colon
    | Dot
    | LeftParen
    | RightParen
    | LessThan
    | GreaterThan
    /// Tuple separator (*).
    | Star
    /// Grouping node — a flat list of child nodes.
    | Node of nodes: TextNode list
    | NewLine
    | OpenTag of string
    | CloseTag of string
    | OpenTagWithClass of string * string
    /// An inline hyperlink; href is used as-is (supports page-local `#anchor` refs).
    | Anchor of text: string * href: string
    | AnchoredProperty of text: string * href: string
    /// An inline hyperlink styled as a keyword (e.g. `new` in a constructor declaration).
    | AnchoredKeyword of text: string * href: string

    member this.TrimStart() =
        let isWhitespace =
            function
            | Space
            | Spaces _
            | NewLine -> true
            | _ -> false

        match this with
        | Space
        | Spaces _
        | NewLine -> Node []
        | Node nodes -> nodes |> List.skipWhile isWhitespace |> Node
        | _ -> this

// ---------------------------------------------------------------------------
// Parameters
// ---------------------------------------------------------------------------

type Parameter =
    {
        Name: string
        Type: TextNode
        /// Signature line without column alignment: `\n    name : type`
        Declaration: TextNode
        /// Signature line with colon aligned to the common column: `\n    name   : type`
        AlignedDeclaration: TextNode
    }

// ---------------------------------------------------------------------------
// Module-level functions and values
// ---------------------------------------------------------------------------

type Function =
    {
        Name: string
        FullName: string
        Signature: TextNode
        /// Curried parameter groups. Each outer list is a curried group;
        /// each inner list holds the parameters within that group (tupled parameters).
        Parameters: Parameter list list
        /// First line without column alignment: `val name :`
        Declaration: TextNode
        /// First line with colon aligned to the common column: `val name   :`
        AlignedDeclaration: TextNode
        /// Generic parameters with constraints, e.g. `<\'T when \'T : comparison>`.
        GenericParameters: TextNode option
        /// Pre-formatted return type with contextual prefix.
        /// With parameters: `\n    -> returnType`
        /// Without parameters: ` returnType`
        ReturnType: TextNode
        XmlDoc: XmlDoc
        ObsoleteInfo: ObsoleteInfo
    }

type Value =
    {
        Name: string
        FullName: string
        Signature: TextNode
        /// Pre-formatted signature line: `val name : type`
        Declaration: TextNode
        /// Generic parameters with constraints, e.g. `<\'T when \'T : comparison>`.
        GenericParameters: TextNode option
        XmlDoc: XmlDoc
        ObsoleteInfo: ObsoleteInfo
    }

// ---------------------------------------------------------------------------
// Entity members (methods, properties, constructors)
// ---------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type MemberKind =
    | Method
    | Property
    | Constructor
    | Operator

type Member =
    {
        Kind: MemberKind
        Name: string
        FullName: string
        /// Curried parameter groups. Each outer list is a curried group;
        /// each inner list holds the parameters within that group (tupled parameters).
        Parameters: Parameter list list
        /// The return type of the member (property type, method return type, or constructed type for constructors).
        ReturnType: TextNode
        /// Pre-formatted signature line for the DocEntry slot,
        /// e.g. `member Add : other : Vector -> Vector` or `property Length : float with get`.
        Declaration: TextNode
        /// Generic parameters with constraints, e.g. `<\'T when \'T : comparison>`.
        GenericParameters: TextNode option
        XmlDoc: XmlDoc
        IsStatic: bool
        /// True for `abstract member` declarations (interface members and abstract class members).
        IsAbstract: bool
        ObsoleteInfo: ObsoleteInfo
    }

// ---------------------------------------------------------------------------
// Record fields
// ---------------------------------------------------------------------------

type Field =
    {
        Name: string
        Type: TextNode
        XmlDoc: XmlDoc
        /// Pre-formatted signature: `field1 : type`
        Declaration: TextNode
    }

    /// Pre-formatted signature: `val field1 : type`
    member this.Signature =
        TextNode.Node
            [
                TextNode.Keyword "val"
                TextNode.Space
                this.Declaration
            ]

// ---------------------------------------------------------------------------
// Union cases
// ---------------------------------------------------------------------------

type UnionCase =
    {
        Name: string
        FullName: string
        Fields: Field list
        XmlDoc: XmlDoc
        /// Pre-formatted signature: `| Name of field1: type1 * field2: type2`
        Declaration: TextNode
    }

// ---------------------------------------------------------------------------
// Entities (types)
// ---------------------------------------------------------------------------

type RecordEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Fields: Field list
        Members: Member list
        /// Pre-formatted full type declaration: `type Name =\n    { field1 : t1\n      ... }`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type UnionEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Cases: UnionCase list
        Members: Member list
        /// Pre-formatted full type declaration with anchor links on case names.
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type AbbrevEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        /// The abbreviated type (right-hand side only).
        Signature: TextNode
        /// Pre-formatted full declaration: `type Name = abbreviatedType`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type ClassEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Members: Member list
        /// Pre-formatted full type declaration with member signatures.
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type InterfaceEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Members: Member list
        /// Pre-formatted full type declaration with member signatures.
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type EnumEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Fields: Field list
        /// Pre-formatted full type declaration: `type Name =\n    | Case1\n    | ...`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
        IsStruct: bool
    }

type MeasureEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        /// Pre-formatted declaration: `[<Measure>] type Name`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
    }

type ExceptionEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        Fields: Field list
        /// Pre-formatted declaration: `exception Name of field1: type1 * field2: type2`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
    }

type DelegateEntity =
    {
        Name: string
        FullName: string
        XmlDoc: XmlDoc
        /// Pre-formatted signature of the Invoke method.
        Signature: TextNode
        /// Pre-formatted declaration: `type Name = delegate of ... -> ...`
        Declaration: TextNode
        ObsoleteInfo: ObsoleteInfo
    }

[<RequireQualifiedAccess>]
type Entity =
    | Record of RecordEntity
    | Union of UnionEntity
    | Abbrev of AbbrevEntity
    | Class of ClassEntity
    | Interface of InterfaceEntity
    | Enum of EnumEntity
    | Measure of MeasureEntity
    | Exception of ExceptionEntity
    | Delegate of DelegateEntity

    member this.Name =
        match this with
        | Record e -> e.Name
        | Union e -> e.Name
        | Abbrev e -> e.Name
        | Class e -> e.Name
        | Interface e -> e.Name
        | Enum e -> e.Name
        | Measure e -> e.Name
        | Exception e -> e.Name
        | Delegate e -> e.Name

    member this.FullName =
        match this with
        | Record e -> e.FullName
        | Union e -> e.FullName
        | Abbrev e -> e.FullName
        | Class e -> e.FullName
        | Interface e -> e.FullName
        | Enum e -> e.FullName
        | Measure e -> e.FullName
        | Exception e -> e.FullName
        | Delegate e -> e.FullName

    member this.XmlDoc =
        match this with
        | Record e -> e.XmlDoc
        | Union e -> e.XmlDoc
        | Abbrev e -> e.XmlDoc
        | Class e -> e.XmlDoc
        | Interface e -> e.XmlDoc
        | Enum e -> e.XmlDoc
        | Measure e -> e.XmlDoc
        | Exception e -> e.XmlDoc
        | Delegate e -> e.XmlDoc

    member this.Declaration =
        match this with
        | Record e -> e.Declaration
        | Union e -> e.Declaration
        | Abbrev e -> e.Declaration
        | Class e -> e.Declaration
        | Interface e -> e.Declaration
        | Enum e -> e.Declaration
        | Measure e -> e.Declaration
        | Exception e -> e.Declaration
        | Delegate e -> e.Declaration

    member this.ObsoleteInfo =
        match this with
        | Record e -> e.ObsoleteInfo
        | Union e -> e.ObsoleteInfo
        | Abbrev e -> e.ObsoleteInfo
        | Class e -> e.ObsoleteInfo
        | Interface e -> e.ObsoleteInfo
        | Enum e -> e.ObsoleteInfo
        | Measure e -> e.ObsoleteInfo
        | Exception e -> e.ObsoleteInfo
        | Delegate e -> e.ObsoleteInfo

    member this.IsStruct =
        match this with
        | Record e -> e.IsStruct
        | Union e -> e.IsStruct
        | Abbrev e -> e.IsStruct
        | Class e -> e.IsStruct
        | Interface e -> e.IsStruct
        | Enum e -> e.IsStruct
        | Measure _ -> false
        | Exception _ -> false
        | Delegate _ -> false

// ---------------------------------------------------------------------------
// Modules
// ---------------------------------------------------------------------------

type Module =
    {
        Name: string
        FullName: string
        /// The parent namespace, Encode.g. "Encode.Geometry" for "Encode.Geometry.Points".
        /// Empty string for root-level modules with no Encode.
        Namespace: string
        XmlDoc: string option
        Entities: Entity list
        Functions: Function list
        Values: Value list
        /// True for synthetic modules that carry bare namespace-level types.
        /// The plugin generates individual entity pages from these rather than a module Encode.
        IsSynthetic: bool
        ObsoleteInfo: ObsoleteInfo
    }

// ---------------------------------------------------------------------------
// Namespaces
// ---------------------------------------------------------------------------

type Namespace =
    {
        /// Short display name, e.g. "global", "Reference", "Geometry".
        Name: string
        /// Fully-qualified name, e.g. "", "Reference", "Reference.Geometry".
        FullName: string
    }

// ---------------------------------------------------------------------------
// Assemblies — top-level root
// ---------------------------------------------------------------------------

type Assembly =
    {
        Name: string
        Namespaces: Namespace list
        Modules: Module list
    }

type Root =
    {
        Assemblies: Assembly list
    }
