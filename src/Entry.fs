module Lit.TodoMVC.Entry

open System
open Elmish
open Lit
open Components

let init() =
    { Todos = []; Edit = None }, Cmd.none

let update msg model =
    match msg with
    | DeleteTodo guid ->
        let todos = model.Todos |> List.filter (fun t -> t.Id <> guid)
        { model with Todos = todos }, Cmd.none

    | ToggleCompleted guid ->
        let todos = model.Todos |> List.map (fun t ->
            if t.Id = guid then { t with Completed = not t.Completed } else t)
        { model with Todos = todos }, Cmd.none

    | StartEdit edit  ->
        { model with Edit = Some edit }, Cmd.none

    | FinishEdit None ->
        { model with Edit = None }, Cmd.none

    | FinishEdit(Some(NewTodo description)) ->
        let todo = { Id = Guid.NewGuid(); Description = description; Completed = false }
        let todos = model.Todos @ [todo]
        { model with Todos = todos; Edit = None }, Cmd.none

    | FinishEdit(Some(EditTodo t1)) ->
        let todos = model.Todos |> List.map (fun t2 ->
            if t1.Id = t2.Id then t1 else t2)
        { model with Todos = todos; Edit = None }, Cmd.none

let view model dispatch =
    html $"""
      <div></div>
    """

open Lit.Elmish
open Lit.Elmish.HMR

Program.mkProgram init update view
|> Program.withLit "app-container"
|> Program.run