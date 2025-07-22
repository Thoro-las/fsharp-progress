module TaskList

open Task

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
    let showTask (index: int) (task: Task) = 
        sprintf "%d: [%s] %s" (index + 1) (getState task.state) task.name

    tasklist |> List.mapi showTask
        |> String.concat "\n"
