module Aornota.Nebulous.Application.Preferences

open Aornota.Nebulous.Literals.Application
open Aornota.Nebulous.Persistence
open Avalonia.Controls
open System

type Preferences =
    { NormalSize: float * float
      NormalLocation: int * int
      WindowState: WindowState }

let private preferencesFile =
    $"{Environment.UserName.ToLowerInvariant()}.{fileExtension Preferences}"

let defaultPreferences =
    { NormalSize = WINDOW_MINIMUM_WIDTH, WINDOW_MINIMUM_HEIGHT
      NormalLocation = 0, 0
      WindowState = WindowState.Normal }

let readPreferences () =
    async { return! read<Preferences> Preferences preferencesFile }

let writePreferences (preferences: Preferences) =
    async { return! write Preferences preferencesFile preferences }
