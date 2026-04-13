namespace FSharp.Oracle

open FSharp.Compiler.Symbols
open FSharp.Oracle.Schema
open Oracle.XmlDoc
open Helpers
open EntityExtractor
open ValueExtractor

module internal ModuleExtractor =
    let extractModule
        (toUrl: string -> string)
        (docs: Map<string, string>)
        (entity: FSharpEntity)
        : Module
        =
        let nested = entity.NestedEntities |> Seq.toList

        let functions, values =
            entity.MembersFunctionsAndValues
            |> Seq.filter (fun m -> not m.IsCompilerGenerated)
            |> Seq.toList
            |> List.partition (fun m -> m.IsFunction)

        let obsoleteInfo = obsoleteOfEntity entity

        let entities =
            nested
            |> List.filter (fun e -> not e.IsFSharpModule)
            |> List.map (extractEntity toUrl docs)

        let funcs = functions |> List.map (extractFunction toUrl docs)
        let vals = values |> List.map (extractValue toUrl docs)

        {
            Name = entity.DisplayName
            FullName = entity.FullName
            Namespace = namespaceOf entity.FullName
            XmlDoc = moduleDocOf docs entity.XmlDocSig
            Entities = entities
            Functions = funcs
            Values = vals
            IsSynthetic = false
            ObsoleteInfo = obsoleteInfo
        }
