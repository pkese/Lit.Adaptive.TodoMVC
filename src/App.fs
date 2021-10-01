module Lit.TodoMVC.App

open System
open Browser.Types
open Browser
open Lit
open FSharp.Data.Adaptive

type Todo = {
    Id: Guid
    Text: string
    Done: bool
} with
    static member create text =
        { Id = Guid.NewGuid()
          Text = text
          Done = false }


// adaptive state

let todos = clist [ Todo.create "Learn F# Adaptive";  Todo.create "Have fun with Lit!" ]
let editing = cval<Guid option> None
let sorted = cval false
let displayTodos = cval false

let orderedTodos = adaptive {
    let! todoList = (todos :> IAdaptiveIndexList<_>).Content
    let! sorted = sorted
    printfn "... now ordering adaptive list: sorted=%b" sorted
    return
        if sorted then
            todoList |> IndexList.sortBy (fun todo -> todo.Text)
        else
            todoList
}


[<HookComponent>]
let NewTodoEl () =
    Hook.useHmr(hmr)
    let inputRef = Hook.useRef<HTMLInputElement>()
    let addNewTodo _ =
        match inputRef.Value with
        | None -> ()
        | Some input ->
            match input.value.Trim() with
            | "" -> ()
            | v -> transact (fun () -> todos.InsertAt(0, Todo.create v)) |> ignore
            input.value <- ""

    html $"""
        <div class="field has-addons">
            <div class="control is-expanded">
                <input {Lit.ref inputRef}
                    type="text"
                    class="input is-medium"
                    aria-label="New todo description"
                    @keyup={Ev(onEnterOrEscape addNewTodo ignore)}>
            </div>
            <div class="control">
                <button class="button is-primary is-medium" aria-label="Add new todo"
                    @click={Ev addNewTodo}>
                    <i class="fa fa-plus"></i>
                </button>
            </div>
        </div>
    """


[<HookComponent>]
let TodoEl (todoIndex:Index) (todo: Todo) =
    Hook.useHmr(hmr)
    let editing, setEditing = Hook.useCVal editing
    let isEditing = editing = Some todo.Id

    let hasFocus = Hook.useRef(false)
    let inputRef = Hook.useRef<HTMLInputElement>()

    Hook.useEffectOnChange(isEditing, function
        | true when not hasFocus.Value ->
            inputRef.Value |> Option.iter (fun i -> i.select())
        | _ -> ())

    let transition =
        Hook.useTransition(
            ms = 500,
            cssBefore = "opacity: 0; transform: scale(2);",
            cssAfter = "opacity: 0; transform: scale(0.1);",
            onComplete = fun isIn -> if not isIn then transact (fun _ -> todos.Remove todoIndex |> ignore)
        )

    let style = transition.css + inline_css """.{
        border: 2px solid lightgray;
        border-radius: 10px;
        margin: 5px 0;
    }"""

    printfn "rendering todo: '%s'" todo.Text

    if isEditing then
        let applyEdit _ = inputRef.Value |> Option.iter (fun input ->
            transact (fun () -> todos.[todoIndex] <- { todo with Text = input.value.Trim() }))
        let cancelEdit _ = setEditing None

        html $"""
            <div class="columns" style={style}>
                <div class="column is-10">
                    <input {Lit.ref inputRef}
                        type="text"
                        class="input"
                        aria-label="Edit todo"
                        value={todo.Text}
                        @keyup={Ev(onEnterOrEscape applyEdit cancelEdit)}
                        @blur={Ev cancelEdit}>
                </div>
                <div class="column is-2">
                    <button class="button is-primary" aria-label="Save edit"
                        @click={Ev applyEdit}>
                        <i class="fa fa-save"></i>
                    </button>
                </div>
            </div>"""
    else
        html $"""
            <div class="columns" style={style}>
                <div class="column is-9">
                    <p class="subtitle"
                        style="cursor: pointer; user-select: none;"
                        @dblclick={Ev(fun _ -> setEditing <| Some todo.Id)}>
                        {todo.Text}
                    </p>
                </div>
                <div class="column is-3">
                    <!-- TODO: Provide aria besides color to indicate if item is complete or not -->
                    <button class={Lit.classes ["button", true; "is-success", todo.Done]}
                        aria-label={if todo.Done then "Mark uncompleted" else "Mark completed"}
                        @click={Ev(fun _ -> transact (fun () -> todos.[todoIndex] <-  { todo with Done = not todo.Done }))}>
                        <i class="fa fa-check"></i>
                    </button>
                    <button class="button is-primary" aria-label="Edit"
                        @click={Ev(fun _ -> setEditing <| Some todo.Id)}>
                        <i class="fa fa-edit"></i>
                    </button>
                    <button class="button is-danger" aria-label="Delete"
                        @click={Ev(fun _ -> transition.triggerLeave())}>
                        <i class="fa fa-times"></i>
                    </button>
                </div>
            </div>
        """

let todosAsAdaptiveHtml : aval<TemplateResult> = adaptive {
    let! todos = orderedTodos
    printfn "... now rendering todos into adaptive html"
    return
        todos
            |> IndexList.mapi (fun i todo -> todo.Id, TodoEl i todo)
            |> Lit.mapUnique (fst >> string) snd
}

[<HookComponent>]
let todoListComponent () =
    let todos = Hook.useAVal todosAsAdaptiveHtml
    html $"""{ todos }"""

[<HookComponent>]
let app () =
    Hook.useHmr(hmr)
    let sorted, setSorted = Hook.useCVal sorted
    let displayTodos, setDisplayTodos = Hook.useCVal displayTodos


    html $"""
      <div style="margin: 0 auto; max-width: 800px; padding: 20px;">
        <p class="title">Lit.TodoMVC</p>
        { NewTodoEl () }
        <input type=checkbox ?checked={sorted} @change={Ev(fun _ -> setSorted (not sorted))} />
        <label>Sort by description</label>
        <br/>
        <input type=checkbox ?checked={displayTodos} @change={Ev(fun _ -> setDisplayTodos (not displayTodos))} />
        <label>Display todos after sorting is applied</label>
        { if displayTodos then todoListComponent() else Lit.nothing }
      </div>
    """

// mount component
Lit.render (document.getElementById "app-container") (app ())
