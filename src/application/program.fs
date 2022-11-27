module Aornota.Nebulous.Application.Program

open Aornota.Nebulous.Application.Model
open Aornota.Nebulous.Application.Preferences
open Aornota.Nebulous.Application.Subscriptions
open Aornota.Nebulous.Application.Transition
open Aornota.Nebulous.Application.View
open Aornota.Nebulous.Literals.Application
open Aornota.Nebulous.Persistence
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Themes.Fluent
open System

type AppWindow(preferences, startupErrors) as this =
    inherit HostWindow()

    do
        base.Title <- applicationNameAndVersion

        base.Icon <- applicationIcon
        base.MinWidth <- WINDOW_MINIMUM_WIDTH
        base.MinHeight <- WINDOW_MINIMUM_HEIGHT
        base.Width <- Math.Max(fst preferences.NormalSize, WINDOW_MINIMUM_WIDTH)
        base.Height <- Math.Max(snd preferences.NormalSize, WINDOW_MINIMUM_HEIGHT)
        base.Position <- PixelPoint(fst preferences.NormalLocation, snd preferences.NormalLocation)
        // TODO-NMB: Why does setting to Maximized not work?...
        base.WindowState <- preferences.WindowState
        this.SystemDecorations <- SystemDecorations.Full

        // this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        // this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let init _ = init preferences startupErrors

#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        let updateWithServices msg state = update msg state this

        Program.mkProgram init updateWithServices view
        |> Program.withHost this
        |> Program.withSubscription (fun _ -> locationChanged this)
        |> Program.withSubscription (fun _ -> effectiveViewportChanged this)
#if DEBUG
        // |> Program.withConsoleTrace
#endif
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let writeDefaultPreferences defaultPreferences =
                match writePreferences defaultPreferences |> Async.RunSynchronously with
                | Ok _ -> []
                | Error error -> [ makeError error (Some "Unable to write default preferences") ]

            let preferences, startupErrors =
                match readPreferences () |> Async.RunSynchronously with
                | Ok preferences -> preferences, []
                | Error FileNotFound -> defaultPreferences, writeDefaultPreferences defaultPreferences
                | Error (Other error) ->
                    defaultPreferences,
                    writeDefaultPreferences defaultPreferences
                    @ [ makeError error (Some "Unable to read preferences") ]

            desktopLifetime.MainWindow <- AppWindow(preferences, startupErrors)
        | _ -> ()

[<EntryPoint>]
let main (args: string[]) =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
