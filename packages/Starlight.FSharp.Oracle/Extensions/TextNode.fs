module TextNode.Extensions

open FSharp.Oracle.Schema

/// Escape characters that have special meaning in JSX expression context.
/// MDX parses HTML as JSX, so `{` and `}` inside tag content must be escaped.
let private escapeJsx (s: string) =
    s.Replace("{", "&#123;").Replace("}", "&#125;")

let wrapWithClass cls (text: string) =
    $"""<span class="{cls}">{escapeJsx text}</span>"""

let wrapInKeyword text = wrapWithClass "fsharp-doc-kw" text

type TextNode with

    static member ToHtml(node: TextNode) : string = node.Html

    static member ToHtml(node: TextNode list) : string = TextNode.Node node |> TextNode.ToHtml

    member this.Html =
        match this with
        | TextNode.Text s -> escapeJsx s
        | TextNode.Colon -> wrapInKeyword ":"
        | TextNode.Arrow -> wrapInKeyword "->"
        | TextNode.Dot -> wrapInKeyword "."
        | TextNode.Comma -> wrapInKeyword ","
        | TextNode.Space -> "&nbsp;"
        | TextNode.GreaterThan -> wrapInKeyword "&gt;"
        | TextNode.LessThan -> wrapInKeyword "&lt;"
        | TextNode.LeftBrace -> wrapInKeyword "{"
        | TextNode.RightBrace -> wrapInKeyword "}"
        | TextNode.Equal -> wrapInKeyword "="
        | TextNode.Tick -> "&#x27;"
        | TextNode.LeftParen -> wrapInKeyword "("
        | TextNode.RightParen -> wrapInKeyword ")"
        | TextNode.Node node -> node |> List.map (fun node -> node.Html) |> String.concat ""
        | TextNode.Keyword text -> wrapInKeyword text
        | TextNode.Star -> wrapInKeyword "*"
        | TextNode.TypeRef(name, _, url) ->
            wrapWithClass "fsharp-doc-type" $"""<a href="{url}">{name}</a>"""
        | TextNode.TypeVar name -> wrapWithClass "fsharp-doc-typevar" name
        | TextNode.NewLine -> "\n"
        | TextNode.OpenTag tagName -> $"""<{tagName}>"""
        | TextNode.OpenTagWithClass(tagName, cls) -> $"""<{tagName} class="{cls}">"""
        | TextNode.CloseTag tagName -> $"""</{tagName}>"""
        | TextNode.Anchor(text, href) -> $"""<a href="{href}">{text}</a>"""
        | TextNode.AnchoredProperty(text, href) ->
            wrapWithClass "fsharp-doc-property" $"""<a href="{href}">{text}</a>"""
        | TextNode.AnchoredKeyword(text, href) ->
            wrapWithClass "fsharp-doc-kw" $"""<a href="{href}">{text}</a>"""
        | TextNode.Spaces count ->
            [
                for _ in 1..count do
                    TextNode.Space
            ]
            |> TextNode.Node
            |> TextNode.ToHtml


