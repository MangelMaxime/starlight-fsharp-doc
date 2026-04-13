module EasyBuild.Commands.Serve

open Spectre.Console.Cli
open SimpleExec
open BlackFox.CommandLine
open System.IO
open EasyBuild.Workspace
open System.ComponentModel
open EasyBuild.Tools.Fable
open EasyBuild.Commands.Docs

type ServeCommand() =

    inherit Command<CommandSettings>()
    interface ICommandLimiter<CommandSettings>

    override __.Execute(context, _, ct) =
        DocsCommand().Execute(context, DocsSettings(), ct) |> ignore

        Command.RunAsync("npx", "http-serve", workingDirectory = VirtualWorkspace.docs.dist.``.``)
        |> Async.AwaitTask
        |> Async.RunSynchronously

        0
