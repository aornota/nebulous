module Aornota.Nebulous.Application.View

open Aornota.Nebulous
open Aornota.Nebulous.Application.Model
open Aornota.Nebulous.Application.Transition
open Aornota.Nebulous.Literals.Colours
open Aornota.Nebulous.Literals.Miscellaneous
open Aornota.Nebulous.Utilities
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open System
open System.Drawing

let private menu state dispatch =
    let applicationMenu =
        MenuItem.create
            [ MenuItem.header "Application"
              MenuItem.fontSize 12.
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header "Exit"
                          MenuItem.fontSize 12.
                          MenuItem.onClick (fun _ -> dispatch OnExit) ] ] ]

    let errorsMenu =
        let hasDebugOnly =
            state.Errors
            |> List.exists (fun (_, _, _, nonDebugMessage) -> nonDebugMessage |> Option.isNone)

        MenuItem.create
            [ MenuItem.header $"Errors ({state.Errors.Length})"
              MenuItem.fontSize 12.
              MenuItem.foreground COLOUR_ERROR
              MenuItem.isEnabled (state.Errors.Length > 0 || state.ShowingDebugOnlyErrors)
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header (
                              if state.ShowingDebugOnlyErrors then
                                  "Hide debug-only"
                              else
                                  "Show debug-only"
                          )
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled (hasDebugOnly || state.ShowingDebugOnlyErrors)
                          MenuItem.onClick (fun _ -> dispatch OnToggleShowingDebugOnlyErrors) ]
                    MenuItem.create
                        [ MenuItem.header "Clear all"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_REMOVE
                          MenuItem.isEnabled (state.Errors.Length > 0)
                          MenuItem.onClick (fun _ -> dispatch OnClearAllErrors) ] ] ]

    Menu.create
        [ Menu.dock Dock.Top
          Menu.viewItems
              [ applicationMenu
                if isDebug then
                    errorsMenu ] ]

let private errorsView showingDebugOnlyErrors (errors: (ErrorId * DateTime * string * string option) list) dispatch =
    let errorTemplate (text, colour: string, errorId: ErrorId option, timestamp: DateTime option) =
        DockPanel.create
            [ DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill true
              DockPanel.children
                  [ match timestamp with
                    | Some timestamp ->
                        TextBlock.create
                            [ TextBlock.dock Dock.Left
                              TextBlock.verticalAlignment VerticalAlignment.Center
                              TextBlock.textAlignment TextAlignment.Left
                              TextBlock.width 170.
                              TextBlock.fontSize 12.
                              TextBlock.text (timestamp.ToString("yyyy-MM-dd 'at' HH:mm:ss.fff")) ]
                    | None -> ()
                    match errorId with
                    | Some errorId ->
                        Button.create
                            [ Button.dock Dock.Right
                              Button.width SIZE_BUTTON_WITH_ICON
                              Button.height SIZE_BUTTON_WITH_ICON
                              Button.background COLOUR_BACKGROUND
                              Button.cornerRadius 0
                              Button.margin (10, 0, 0, 0)
                              Button.content (Icons.remove true (Some COLOUR_REMOVE) None)
                              Button.tip "Dismiss error"
                              Button.onClick (fun _ -> dispatch (OnDismissError errorId)) ]
                    | None -> ()
                    TextBlock.create
                        [ TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Left
                          TextBlock.fontSize 12.
                          TextBlock.foreground colour
                          TextBlock.text text ] ] ]

    let errors =
        if isDebug && showingDebugOnlyErrors then
            errors
        else
            errors
            |> List.filter (fun (_, _, _, nonDebugMessage) -> nonDebugMessage |> Option.isSome)

    let errorsForView =
        errors
        |> List.collect (fun (errorId, timestamp, error, nonDebugMessage) ->
            match nonDebugMessage with
            | Some nonDebugMessage ->
                if isDebug then
                    [ nonDebugMessage, COLOUR_INACTIVE, Some errorId, None
                      error, COLOUR_ERROR, None, Some timestamp ]
                else
                    [ nonDebugMessage, COLOUR_INACTIVE, Some errorId, None ]
            | None -> [ error, COLOUR_ERROR, Some errorId, Some timestamp ])

    match errors, isDebug && showingDebugOnlyErrors with
    | _ :: _, _ ->
        ListBox.create
            [ ListBox.dock Dock.Top
              ListBox.maxHeight 170.
              ListBox.background COLOUR_BACKGROUND
              ListBox.dataItems errorsForView
              ListBox.itemTemplate (
                  DataTemplateView<string * string * ErrorId option * DateTime option>.create
                      (fun errorForView -> errorTemplate errorForView)
              ) ]
        :> IView
        |> Some
    | [], true ->
        TextBlock.create
            [ TextBlock.dock Dock.Top
              TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.horizontalAlignment HorizontalAlignment.Left
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.padding (10, 0, 0, 0)
              TextBlock.fontSize 12.
              TextBlock.foreground COLOUR_DISABLED_TEXT
              TextBlock.text "- no debug-only errors -" ]
        :> IView
        |> Some
    | [], false -> None

let view state dispatch =
    DockPanel.create
        [ DockPanel.verticalAlignment VerticalAlignment.Stretch
          DockPanel.horizontalAlignment HorizontalAlignment.Stretch
          DockPanel.lastChildFill true
          DockPanel.children
              [ menu state dispatch
                match errorsView state.ShowingDebugOnlyErrors state.Errors dispatch with
                | Some errorsView -> errorsView
                | None -> ()
                yield! Simulation.View.view state.SimulationState (SimulationMsg >> dispatch) ] ]
