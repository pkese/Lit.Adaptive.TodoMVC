module Lit.TodoMVC

open FSharp.Data.Adaptive
open Browser.Types
open Lit


let hmr = HMR.createToken()

let onEnterOrEscape onEnter onEscape (ev: Event) =
    let ev = ev :?> KeyboardEvent
    match ev.key with
    | "Enter" -> onEnter ev
    | "Escape" -> onEscape ev
    | _ -> ()

type Lit.Hook with

    /// Use adaptive value (re-render component when value changes)
    static member inline useAVal(v: aval<'T>): 'T =
        let ctx = Hook.getContext()
        let state, setState = ctx.useState (fun () -> AVal.force v)
        ctx.useEffectOnce(fun () -> v.AddCallback(fun _ -> setState(AVal.force v)))
        state

    /// Use changeable adaptive value (re-render component when value changes)
    static member inline useCVal(v: cval<'T>): 'T * ('T -> unit) =
        let ctx = Hook.getContext()
        let state, setState = ctx.useState (fun () -> AVal.force v)
        ctx.useEffectOnce(fun () -> v.AddCallback(fun _ -> setState(AVal.force v)))
        let setCState newVal = transact (fun () -> v.Value <- newVal)
        state, setCState

