module EasyBuild.Commands.Docs

open Spectre.Console.Cli
open SimpleExec
open EasyBuild.Workspace
open EasyBuild.Tools.Fable
open EasyBuild.Tools.Nodemon

type DocsSettings() =

    inherit CommandSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type DocsCommand() =
    inherit Command<DocsSettings>()
    interface ICommandLimiter<DocsSettings>

    override __.Execute(_, settings, _) =

        if settings.IsWatch then
            [
                Fable.watch (
                    outDir = VirtualWorkspace.dist.``.``,
                    workingDirectory = Workspace.packages.``Starlight.FSharp.Oracle``.``.``
                )
                |> Async.AwaitTask

                Command.RunAsync("npx", "astro dev", workingDirectory = Workspace.docs.``.``)
                |> Async.AwaitTask
            ]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

        else

            Fable.build (
                outDir = VirtualWorkspace.dist.``.``,
                workingDirectory = Workspace.packages.``Starlight.FSharp.Oracle``.``.``
            )

            // Need to re-install to make sure we use the version of the tool we just compiled above
            Command.Run("npx", "pnpm install")

            Command.Run("npx", "astro build", workingDirectory = Workspace.docs.``.``)

        0
