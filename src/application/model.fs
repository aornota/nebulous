module Aornota.Nebulous.Application.Model

open Aornota.Nebulous
open Aornota.Nebulous.Literals.Application
open Aornota.Nebulous.Literals.FileExtensions
open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging
open Avalonia.Platform
open System

[<Literal>]
let private ASSETS_IMAGES = "avares://nebulous/assets/images/"

type Bitmap with

    static member FromImageAsset(name: string) : IBitmap =
        new Bitmap(
            AvaloniaLocator
                .Current
                .GetService<IAssetLoader>()
                .Open(Uri($"{ASSETS_IMAGES}{name}", UriKind.RelativeOrAbsolute))
        )

type ErrorId =
    | ErrorId of Guid

    static member Create() = ErrorId(Guid.NewGuid())

type WritePreferencesRequestId =
    | WritePreferencesRequestId of Guid

    static member Create() =
        WritePreferencesRequestId(Guid.NewGuid())

type WritePreferencesRequestSource = | Host

type State =
    { ShowingDebugOnlyErrors: bool
      Errors: (ErrorId * DateTime * string * string option) list
      LastNormalSize: float * float
      LastNormalLocation: int * int
      LastWindowState: WindowState
      WritePreferencesRequests: (WritePreferencesRequestId * WritePreferencesRequestSource) list
      SimulationState: Simulation.Model.State }

let makeError error nonDebugMessage =
    ErrorId.Create(), DateTime.UtcNow, error, nonDebugMessage

let applicationIcon =
    WindowIcon(Bitmap.FromImageAsset $"{APPLICATION_NAME}.{FILE_EXTENSION_PNG}")

let applicationNameAndVersion = $"{APPLICATION_NAME} ({APPLICATION_VERSION})"
