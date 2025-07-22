open Task
open TaskList
open System

let prompt (prefix: string) : string = 
    printf "%s " prefix
    Console.ReadLine().Trim()

let promptIndex (prefix: string) (xs: 'a list) : int option =
    let valid, index  = Int32.TryParse (prompt prefix)
    if not valid then None
    else
        if index < 1 || index > xs.Length + 1 then None
        else Some (index - 1)

let inline pause() = while Console.ReadKey(true).Key <> ConsoleKey.Q do ()

let rec editTaskList (tasklist: TaskList) : TaskList =
    Console.Clear()
    printfn "%s" (displayTaskList tasklist)

    printfn "\n| add: Add Task\n| remove: Remove Task\n| edit: Edit Task\n| details: Show Task Details\n| save: Save Task List"
    let action: string = prompt "| |>"
    if action = "save" then tasklist
    else
        let newtasklist =
            match action with
                | "add" ->
                    printfn "Add Task"
                    let name: string = prompt "|> Name:" 
                    let description: string = prompt "|> Description:"
                    tasklist |> addTask {name = name; desc = description; state = ToDo}

                | "remove" ->
                    printfn "Remove Task"
                    match promptIndex "|> Index:" tasklist with
                        | Some index -> tasklist |> removeTask index
                        | None -> tasklist

                | "edit" ->
                    printfn "Edit Task"
                    let index = promptIndex "|> Index:" tasklist
                    let name: string = prompt "|> Name:"
                    let description: string = prompt "|> Description:"
                    let operation = fun (task: Task) -> {task with name = name; desc = description}

                    match index with
                        | Some n -> tasklist |> editTask n operation
                        | None -> tasklist

                | "details" ->
                    printfn "Show Details"
                    let index = promptIndex "|> Index:" tasklist
                    match index with
                        | None -> tasklist
                        | Some n ->
                            let task: Task = tasklist.[n]
                            printfn "\n>> Name: %s\n>> Description: %s\n\n\n(Q) to quit" task.name task.desc
                            pause()

                            tasklist
                | _ ->
                    tasklist

        editTaskList newtasklist

[<EntryPoint>]
let main _ =
    let tasklist: TaskList = [
        {name = "First"; desc = "First description"; state = ToDo}
        {name = "Second"; desc = "Second description"; state = Done}
        {name = "Third"; desc = "Third description"; state = ToDo}
    ]
    
    ignore (editTaskList tasklist)

    0
