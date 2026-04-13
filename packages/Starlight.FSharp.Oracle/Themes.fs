module Starlight.FSharp.Themes

open Fable.Core
open Fable.Core.JsInterop
open System.Text
open StringBuilder.Extensions

[<Emit("Array.isArray($0)")>]
let private isArray (x: obj) : bool = jsNative

let private scopesOf (rule: obj) : string array =
    let scope: obj = rule?scope

    if isNull (box scope) then
        [||]
    elif isArray scope then
        scope :?> string array
    else
        [| scope :?> string |]

let private colorFor (theme: obj) (targetScopes: string list) : string =
    let settings: obj array = theme?settings
    let fg: string = theme?fg

    // A rule applies to a token when the token's scope starts with the rule's scope.
    // We look for the first rule whose scope is a prefix of any of our target scopes.
    settings
    |> Array.tryPick (fun rule ->
        let ruleScopes = scopesOf rule

        let matches =
            targetScopes
            |> List.exists (fun target ->
                ruleScopes
                |> Array.exists (fun rs -> target = rs || target.StartsWith(rs + "."))
            )

        if not matches then
            None
        else
            let color: obj = rule?settings?foreground

            if isNull (box color) then
                None
            else
                Some(color :?> string)
    )
    |> Option.defaultValue fg

/// Generates CSS custom-property declarations for each syntax-highlighting
/// theme extracted from the Expressive Code renderer. The static class rules
/// (colour assignments, layout, sidebar badges, etc.) now live in
/// components/fsharp-doc.css and are imported by FSharpDocPage.astro.
let generateCss (ecRenderer: obj) : string =
    let themes: obj array = ecRenderer?ec?themes
    let sb = StringBuilder()

    for theme in themes do
        let themeType: string = theme?``type``

        let selector =
            if themeType = "dark" then
                ":root[data-theme='dark']"
            else
                ":root"

        let kw =
            colorFor
                theme
                [
                    "keyword"
                    "storage.type"
                    "keyword.operator"
                ]

        let fn_ =
            colorFor
                theme
                [
                    "variable"
                    "entity.name.function"
                ]

        let type_ =
            colorFor
                theme
                [
                    "entity"
                    "entity.name"
                ]

        let tvar =
            colorFor
                theme
                [
                    "entity"
                    "entity.name"
                ]

        sb.WriteLine($"{selector} {{")
        sb.WriteLine($"    --fsharp-doc-kw: {kw};")
        sb.WriteLine($"    --fsharp-doc-fn: {fn_};")
        sb.WriteLine($"    --fsharp-doc-type: {type_};")
        sb.WriteLine($"    --fsharp-doc-typevar: {tvar};")
        sb.WriteLine("}")

    sb.ToString()
