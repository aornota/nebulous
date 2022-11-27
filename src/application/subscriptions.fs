module Aornota.Nebulous.Application.Subscriptions

open Avalonia.FuncUI.Hosts
open Elmish
open System

let locationChanged (window: HostWindow) =
    let sub dispatch =
        window.PositionChanged.Subscribe(fun _ -> dispatch Transition.Msg.LocationChanged)
        |> ignore

    Cmd.ofSub sub

let effectiveViewportChanged (window: HostWindow) =
    let sub dispatch =
        window.EffectiveViewportChanged.Subscribe(fun _ -> dispatch Transition.Msg.EffectiveViewportChanged)
        |> ignore

    Cmd.ofSub sub
