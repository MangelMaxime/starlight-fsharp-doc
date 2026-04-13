module EasyBuild.Commands.Docs

open Spectre.Console.Cli
open SimpleExec
open BlackFox.CommandLine
open System.IO
open EasyBuild.Workspace
open System.ComponentModel
open EasyBuild.Tools.Fable

type DocsSettings() =

    inherit CommandSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type DocsCommand() =
    inherit Command<DocsSettings>()
    interface ICommandLimiter<DocsSettings>

    override __.Execute(_, settings, _) =
        if settings.IsWatch then
            [ async { return 1 } ] |> Async.Parallel |> Async.RunSynchronously |> ignore

        else

            Fable.build (workingDirectory = Workspace.packages.``Starlight.FSharp.Oracle``.``.``)

            Command.Run("npx", "astro build", workingDirectory = Workspace.example.``.``)

        0
