module TaskList

open Task
open System.IO
open FSharp.Json

type TaskList = Task list

let inRange (i: int) (xs: 'a list) = i >= 0 || i <= xs.Length
let applyDefault (map: 'a list -> 'a list) (predicate: 'a list -> bool) (xs: 'a list) = 
    if predicate xs then map xs else xs

let addTask (task: Task) (tasklist: TaskList) : TaskList =
    tasklist |> applyDefault (
        fun tasklist -> task :: tasklist
    ) (not << List.contains task)

let removeTask (i: int) (tasklist: TaskList) : TaskList =
    tasklist |> applyDefault (
        List.mapi (fun index task -> task, index)
            >> List.filter (fun (_, j) -> j <> i)
            >> List.map fst
    ) (inRange i)

let getTask (i: int) (tasklist: TaskList) : Task option =
    match inRange i tasklist with
        | true -> Some tasklist.[i]
        | false -> None

let moveTask (i: int) (j: int) (tasklist: TaskList) : TaskList =
    tasklist |> applyDefault (
        fun tasklist -> 
            let item = tasklist.[i]
            tasklist |> List.removeAt i |> List.insertAt j item
    ) (fun xs -> inRange i xs && inRange j xs)

let editTask (i: int) (operation: Task -> Task) (tasklist: TaskList) : TaskList =
    tasklist |> applyDefault (
        List.mapi (fun index task ->
            if i = index then operation task
            else task
        )
    ) (inRange i)

let setTaskState (i: int) (state: TaskState) (tasklist: TaskList) : TaskList =
    tasklist |> applyDefault (
        List.mapi (fun index task ->
            if i = index then {task with state = state}
            else task
        )
    ) (inRange i)


let displayTaskList (tasklist: TaskList) : string =
    if tasklist = [] then "The tasklist is empty"
    else 
        let showTask (index: int) (task: Task) = 
            sprintf "%d: [%s] %s" (index + 1) (getStateSymbol task.state) task.name

        tasklist |> List.mapi showTask
            |> String.concat "\n"

let loadTaskList (path: string) : TaskList =
    let targetPath = Path.Combine(Directory.GetCurrentDirectory(), "out/", path)
    if not (File.Exists targetPath) then []
    else File.ReadAllText targetPath |> Json.deserialize<TaskList>

let saveTaskList (path: string) (tasklist: TaskList) : TaskList =
    let serializedTaskList = Json.serialize tasklist
    let targetPath = Path.Combine(Directory.GetCurrentDirectory(), "out/", path)
    File.WriteAllText(targetPath, serializedTaskList)
    tasklist

