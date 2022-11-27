module Aornota.Nebulous.Application.Transition

open Aornota.Nebulous
open Aornota.Nebulous.Application.Model
open Aornota.Nebulous.Application.Preferences
open Aornota.Nebulous.Utilities
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Hosts
open Elmish
open System

type Msg =
    // Internal
    | NoOp
    | WritePreferences of WritePreferencesRequestSource
    | DebounceWritePreferencesRequest of WritePreferencesRequestId * WritePreferencesRequestSource
    | AddError of string * string option
    // UI
    | OnExit
    | OnToggleShowingDebugOnlyErrors
    | OnClearAllErrors
    | OnDismissError of ErrorId
    // From Simulation
    | SimulationMsg of Simulation.Transition.Msg
    // From HostWindow subscriptions
    | LocationChanged
    | EffectiveViewportChanged

let private errorOrNoOp nonDebugMessage =
    function
    | Ok _ -> NoOp
    | Error error -> AddError(error, nonDebugMessage)

let init preferences (startupErrors: (ErrorId * DateTime * string * string option) list) =
    let simulationState, simulationMsgs = Simulation.Transition.init ()

    { ShowingDebugOnlyErrors = isDebug && startupErrors.Length > 0
      Errors = startupErrors
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
      WritePreferencesRequests = []
      SimulationState = simulationState },
    Cmd.batch [ yield! simulationMsgs |> List.map (Cmd.ofMsg >> (Cmd.map SimulationMsg)) ]

let update msg (state: State) (window: HostWindow) =
    let noChange = state, Cmd.none

    match msg with
    // Internal
    | NoOp -> noChange
    | WritePreferences source ->
        let writePreferencesRequest = WritePreferencesRequestId.Create(), source

        let delay () =
            async {
                do! Async.Sleep 500
                return writePreferencesRequest
            }

        { state with WritePreferencesRequests = writePreferencesRequest :: state.WritePreferencesRequests },
        Cmd.OfAsync.perform delay () DebounceWritePreferencesRequest
    | DebounceWritePreferencesRequest (writePreferencesRequestId, source) ->
        let updatedWritePreferencesRequests =
            state.WritePreferencesRequests
            |> List.filter (fun (existingWritePreferencesRequestId, _) ->
                existingWritePreferencesRequestId <> writePreferencesRequestId)

        match
            updatedWritePreferencesRequests
            |> List.filter (fun (_, existingSource) -> existingSource = source)
        with
        | _ :: _ -> { state with WritePreferencesRequests = updatedWritePreferencesRequests }, Cmd.none
        | [] ->
            let write =
                match source with
                | Host ->
                    (window.WindowState = WindowState.Normal
                     && ((window.Width, window.Height) <> state.LastNormalSize
                         || (window.Position.X, window.Position.Y) <> state.LastNormalLocation))
                    || (window.WindowState <> WindowState.Minimized
                        && window.WindowState <> state.LastWindowState)

            if write then
                let preferences =
                    let isNormal = window.WindowState = WindowState.Normal

                    { NormalSize =
                        if isNormal then
                            window.Width, window.Height
                        else
                            state.LastNormalSize
                      NormalLocation =
                        if isNormal then
                            window.Position.X, window.Position.Y
                        else
                            state.LastNormalLocation
                      WindowState = window.WindowState }

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    WritePreferencesRequests = updatedWritePreferencesRequests },
                Cmd.OfAsync.perform writePreferences preferences (errorOrNoOp (Some "Unable to write preferences"))
            else
                { state with WritePreferencesRequests = updatedWritePreferencesRequests }, Cmd.none
    | AddError (error, nonDebugMessage) ->
        let maxErrors = 100

        let updatedErrors =
            makeError error nonDebugMessage
            :: (if state.Errors.Length > (maxErrors - 1) then
                    state.Errors |> List.take (maxErrors - 1)
                else
                    state.Errors)

        { state with Errors = updatedErrors }, Cmd.none
    // UI
    | OnExit ->
        match Avalonia.Application.Current.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.Shutdown 0
        | _ -> ()

        noChange
    | OnToggleShowingDebugOnlyErrors ->
        { state with ShowingDebugOnlyErrors = not state.ShowingDebugOnlyErrors }, Cmd.none
    | OnClearAllErrors -> { state with Errors = [] }, Cmd.none
    | OnDismissError errorId ->
        let updatedErrors =
            state.Errors
            |> List.filter (fun (existingErrorId, _, _, _) -> existingErrorId <> errorId)

        { state with Errors = updatedErrors }, Cmd.none
    // From Simulation
    | SimulationMsg simulationMsg ->
        let handleExternal externalMsg =
            match externalMsg with
            | Simulation.Transition.ExternalMsg.NotifyError (error, nonDebugMessage) ->
                Cmd.ofMsg (AddError(error, nonDebugMessage))

        let updatedSimulationState, cmd, externalMsgs =
            Simulation.Transition.update simulationMsg state.SimulationState

        { state with SimulationState = updatedSimulationState },
        Cmd.batch [ Cmd.map SimulationMsg cmd; yield! externalMsgs |> List.map handleExternal ]
    // From HostWindow subscriptions
    | LocationChanged -> state, Cmd.ofMsg (WritePreferences Host)
    | EffectiveViewportChanged -> state, Cmd.ofMsg (WritePreferences Host)
