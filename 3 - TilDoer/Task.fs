module Task

type TaskState =
    | ToDo
    | Aborted
    | Doing
    | Done

let getStateSymbol (state: TaskState) : string =
    match state with
        | ToDo -> " "
        | Aborted -> ""
        | Doing -> "󰔚"
        | Done -> ""

type Task = {
    name: string;
    desc: string;
    state: TaskState;
}

let getTask (task: Task) (showDesc: bool) : string =
    let state = getStateSymbol task.state
    let name = task.name
    let desc = if showDesc then "\n > " + task.desc else ""

    sprintf "[%s] %s%s" state name desc
