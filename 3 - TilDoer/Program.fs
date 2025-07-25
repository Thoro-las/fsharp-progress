module Program

open Task
open TaskList
open Interface.Menus
open Interface.Inputs

let addTaskAction = 
    MenuItem "add" "adds a task to the tasklist"
        <| fun tasklist ->
            let values = 
                createForm "add task"
                    +> TextField "name"
                    +> TextField "description"
                |> promptForm

            match values with
                | ["name", Text name; "description", Text description] -> 
                    let task: Task = {
                        name = name;
                        desc = description;
                        state = ToDo
                    }

                    tasklist |> addTask task, "task added"
                | _ -> tasklist, "could not add task"

let removeTaskAction =
    MenuItem "remove" "removes a task from the tasklist"
        <| fun (tasklist: TaskList) ->
            let values = 
                createForm "remove task"
                    +> IndexField "index" tasklist.Length
                    +> TextField "are you sure"
                |> promptForm

            match values with
                | ["index", Index i; "are you sure", Text "yes"] ->
                    tasklist |> removeTask (i - 1), "task removed"
                | _ -> tasklist, "could not remove task"

let setStateAction =
    MenuItem "state" "change task state"
        <| fun (tasklist: TaskList) ->
            let states = [ToDo; Aborted; Doing; Done]
            let statesText = "state [" + (
                states |> List.mapi (fun i x -> $"{i}: {x}") 
                       |> String.concat ", " ) + "]"
            let values =
                createForm "change state"
                    +> IndexField "index" tasklist.Length
                    +> IndexField statesText states.Length
                |> promptForm

            match values with
                | ["index", Index i; _, Index j] ->
                    tasklist |> editTask (i - 1) (fun (task: Task) -> {task with state = states.[j]}), $"changed state to {states.[j].ToString()}"
                | _ -> tasklist, "could not change the state of the task"

let editTaskAction =
    MenuItem "edit" "edit task values"
        <| fun (tasklist: TaskList) ->
            let values =
                createForm "edit task"
                    +> IndexField "index" tasklist.Length
                    +> TextField "name"
                    +> TextField "description"
                |> promptForm

            match values with
                | ["index", Index i; "name", Text name; "description", Text desc] ->
                    let newName = fun (task: Task) -> if name = "" then task.name else name
                    let newDescription = fun (task: Task) -> if desc = "" then task.desc else desc
                    tasklist |> editTask (i - 1) (fun (task: Task) -> {task with name = newName task; desc = newDescription task}), "edit task successfully"
                | _ -> tasklist, "could not edit the task"

let moveTaskAction =
    MenuItem "move" "moves the task to other places"
        <| fun (tasklist: TaskList) ->
            let values =
                createForm "move task"
                    +> IndexField "index" tasklist.Length
                    +> IndexField "to" tasklist.Length
                |> promptForm

            match values with
                | ["index", Index i; "to", Index j] ->
                    tasklist |> moveTask (i - 1) (j - 1), $"task moved from {i} to {j}"
                | _ -> tasklist, "could not move the tasks"

let taskDetailsAction =
    MenuItem "details" "show task details"
        <| fun (tasklist: TaskList) ->
            let values =
                createForm "show task details"
                    +> IndexField "index" tasklist.Length
                |> promptForm

            match values with
                | ["index", Index i] ->
                    let task: Task = tasklist.[(i - 1)]
                    tasklist, sprintf "\n>> name: %s\n>> description: %s\n" task.name task.desc
                | _ -> tasklist, "could not show task details"

let saveTaskListAction =
    MenuItem "save" "save the current tasklist"
        <| fun (tasklist: TaskList) ->
            let values =
                createForm "save tasklist"
                    +> TextField "path"
                |> promptForm

            match values with
                | ["path", Text path] ->
                    tasklist |> saveTaskList path, $"saving to ({path})"
                | _ -> tasklist, "could not save"

let loadTaskListAction =
    MenuItem "load" "load a tasklist"
        <| fun (tasklist: TaskList) ->
            let values =
                createForm "load tasklist"
                    +> TextField "path"
                |> promptForm

            match values with
                | ["path", Text path] ->
                    loadTaskList path, $"loading from ({path})"
                | _ -> tasklist, "could not load"

[<EntryPoint>]
let main _ =
    let tasklist: TaskList = []

    let menu = 
        createMenu<TaskList> "Main Menu"
            ++ addTaskAction
            ++ removeTaskAction
            ++ setStateAction
            ++ moveTaskAction
            ++ editTaskAction
            ++ taskDetailsAction
            ++ saveTaskListAction
            ++ loadTaskListAction
        |> addHelpItem

    showMenu tasklist displayTaskList menu
    0
