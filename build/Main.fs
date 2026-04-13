module EasyBuild.Main

open Spectre.Console.Cli
open EasyBuild.Commands.Docs
open SimpleExec
open EasyBuild.Tools.Husky
open EasyBuild.Tools.Npm

[<EntryPoint>]
let main args =

    Husky.install()

    Command.Run("npx", "pnpm install")

    let app = CommandApp()

    app.Configure(fun config ->
        config.Settings.ApplicationName <- "./build.sh"

        config
            .AddCommand<DocsCommand>("docs")
        |> ignore
    )

    app.Run(args)
