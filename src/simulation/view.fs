module Aornota.Nebulous.Simulation.View

open Aornota.Nebulous
open Aornota.Nebulous.Simulation.Configuration
open Aornota.Nebulous.Simulation.Model
open Aornota.Nebulous.Simulation.Transition
open Aornota.Nebulous.Literals.Colours
open Aornota.Nebulous.Literals.Miscellaneous
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

let private button<'a>
    (fIcon: bool -> string option -> string option -> IView<Canvas>)
    dock
    enabled
    enabledColourOverride
    disabledColourOverride
    leftMargin
    (tip: string)
    onClick
    =
    Button.create
        [ match dock with
          | Some dock -> Button.dock dock
          | None -> ()
          Button.width SIZE_BUTTON_WITH_ICON
          Button.height SIZE_BUTTON_WITH_ICON
          Button.background COLOUR_BACKGROUND
          Button.margin (leftMargin, 0, 0, 0)
          Button.cornerRadius 0
          Button.content (fIcon enabled enabledColourOverride disabledColourOverride)
          Button.tip (if enabled then tip else "")
          Button.onClick (if enabled then onClick else ignore) ]

let private leftInfo state =
    let configurationNasceneAndExpiry =
        let configuration, fromGeneration = state.Configuration

        if
            (not state.PendingValidateConfiguration && state.Evolving <> None)
            || (configuration |> validate).Length = 0
        then
            let nascencePercent =
                100.
                * nascenceRatio configuration.MaxHealthForNascence configuration.NascenceRateDivisor

            let expiry =
                match configuration.ExpiresAfter with
                | Some value ->
                    let remaining = value - (state.Generation - fromGeneration)

                    $"{remaining}"
                | None -> "N/A"

            $"%.2f{nascencePercent}%% ({expiry})"
        else
            "(invalid)"

    let text = $"{state.Generation}  |  {configurationNasceneAndExpiry}"

    TextBlock.create
        [ TextBlock.dock Dock.Left
          TextBlock.verticalAlignment VerticalAlignment.Center
          TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.textAlignment TextAlignment.Left
          TextBlock.width 180
          TextBlock.margin (15, 0, 0, 0)
          TextBlock.fontSize 12
          TextBlock.foreground COLOUR_INACTIVE
          TextBlock.text text ]

let private rightInfo state =
    let width, height = state.Dimensions

    let configurationPartitions =
        let configuration, fromGeneration = state.Configuration

        if
            (not state.PendingValidateConfiguration && state.Evolving <> None)
            || (configuration |> validate).Length = 0
        then
            $" ({configuration.HorizontalPartitions}x{configuration.VerticalPartitions})"
        else
            ""

    let text = $"{width}x{height}{configurationPartitions}"

    TextBlock.create
        [ TextBlock.dock Dock.Right
          TextBlock.verticalAlignment VerticalAlignment.Center
          TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.textAlignment TextAlignment.Right
          TextBlock.width 180
          TextBlock.margin (0, 0, 15, 0)
          TextBlock.fontSize 12
          TextBlock.foreground COLOUR_INACTIVE
          TextBlock.text text ]

let private controls state dispatch =
    let toggleEvolveEnabled, toggleEvolveColour, toggleEvolveDisabledColourOverride, toggleEvolveIcon, toggleEvolveTip =
        let startOrResume =
            $"""{if state.Generation = INITIAL_GENERATION then
                     "Start"
                 else
                     "Resume"}"""

        match state.Evolving with
        | Some true -> true, COLOUR_AWAITING, None, Icons.pause, "Pause"
        | Some false -> true, COLOUR_ACTIVE, None, Icons.play, startOrResume
        | None -> state.PendingValidateConfiguration, COLOUR_ACTIVE, Some COLOUR_ERROR, Icons.play, startOrResume

    let rateIcon =
        match state.Rate with
        | rate when rate < (MAX_RATE / 3) -> Icons.rateLow
        | rate when rate < (MAX_RATE * 2 / 3) -> Icons.rateMedium
        | _ -> Icons.rateHigh

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.margin (0, 0, 0, 0)
          StackPanel.children
              [ button
                    Icons.showBackgroundOnly
                    None
                    (state.Showing <> BackgroundOnly)
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    0
                    "Show background only"
                    (fun _ -> dispatch OnShowBackgroundOnly)
                button
                    Icons.showBackgroundAndForeground
                    None
                    (state.Showing <> BackgroundAndForeground)
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    6
                    "Show background and foreground"
                    (fun _ -> dispatch OnShowBackgroundAndForeground)
                button
                    Icons.showForegroundOnly
                    None
                    (state.Showing <> ForegroundOnly)
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    6
                    "Show foreground only"
                    (fun _ -> dispatch OnShowForegroundOnly)
                button
                    toggleEvolveIcon
                    None
                    toggleEvolveEnabled
                    (Some toggleEvolveColour)
                    toggleEvolveDisabledColourOverride
                    12
                    toggleEvolveTip
                    (fun _ -> dispatch OnToggleEvolve)
                button
                    Icons.reset
                    None
                    (state.Generation > INITIAL_GENERATION)
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    6
                    "Reset"
                    (fun _ -> dispatch OnReset)
                button
                    Icons.randomConfiguration
                    None
                    true
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    12
                    "Random configuration"
                    (fun _ -> dispatch OnRandomConfiguration)
                button
                    Icons.save
                    None
                    (state.Generation > INITIAL_GENERATION && state.Evolving <> Some true)
                    (Some COLOUR_INACTIVE)
                    (Some COLOUR_DISABLED_ICON)
                    12
                    "Save image"
                    (fun _ -> dispatch OnSave)
                button rateIcon None false (Some COLOUR_RATE) (Some COLOUR_RATE) 20 "" ignore
                Slider.create
                    [ Slider.verticalAlignment VerticalAlignment.Center
                      Slider.horizontalAlignment HorizontalAlignment.Center
                      Slider.width 100.
                      Slider.minimum MIN_RATE
                      Slider.maximum MAX_RATE
                      Slider.margin (8, 0, 0, 0)
                      Slider.padding (0, 0, 0, 6)
                      Slider.foreground COLOUR_RATE
                      Slider.value state.Rate
                      Slider.tip $"Rate: {state.Rate}%%"
                      Slider.onValueChanged (fun value -> dispatch (OnRate(int value))) ] ] ]

let view state dispatch =
    [ DockPanel.create
          [ DockPanel.dock Dock.Bottom
            DockPanel.lastChildFill true
            DockPanel.children [ leftInfo state; rightInfo state; controls state dispatch ] ]
      :> IView
      Image.create
          [ Image.verticalAlignment VerticalAlignment.Center
            Image.horizontalAlignment HorizontalAlignment.Center
            Image.margin (10, 10, 10, 10)
            Image.stretch Stretch.Fill
            Image.source (asAvaloniaBitmap state) ] ]
