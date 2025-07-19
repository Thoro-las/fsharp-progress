open System

type Cell = int
type Grid = { size: int; cells: Cell list list }
type Player = { x: int; y: int }

let RNG = Random()
let colors: string list = [ "🟥"; "🟩"; "🟦"; "⬛" ]

let generateCell () : Cell = RNG.Next 100 * colors.Length / 100

let generateGrid (size: int) : Grid =
    { size = size
      cells = [ for _ in 0 .. size - 1 -> [ for _ in 0 .. size - 1 -> generateCell () ] ] }

let displayCell (cell: Cell) = printf "%s" colors.[cell]

let displayGrid (grid: Grid) (player: Player) =
    let size, cells = grid.size, grid.cells

    for j in 0 .. size - 1 do
        for i in 0 .. size - 1 do
            if i = player.x && j = player.y then
                printf " ["
                displayCell cells.[i].[j]
                printf "]"
            else
                printf "  "
                displayCell cells.[i].[j]
                printf " "

        printf "\n\n"

let clearScreen () = Console.Clear()

let clamp x min max =
    if x < min then min
    elif x >= max then max - 1
    else x

let toggleCell (position: int * int) (grid: Grid) : Grid =
    let x, y = position

    { size = grid.size
      cells =
        grid.cells
        |> List.mapi (fun j row ->
            if j = x then
                row
                |> List.mapi (fun i cell -> if i = y then (cell + 1) % colors.Length else cell)
            else
                row) }

let affectGrid (key: string) (grid: Grid) (player: Player) : Grid =
    if not (key = "Spacebar") then
        grid
    else
        let x = player.x
        let y = player.y

        let minx = clamp (x - 1) 0 grid.size
        let maxx = clamp (x + 1) 0 grid.size
        let miny = clamp (y - 1) 0 grid.size
        let maxy = clamp (y + 1) 0 grid.size

        grid
        |> toggleCell (minx, y)
        |> toggleCell (maxx, y)
        |> toggleCell (x, miny)
        |> toggleCell (x, maxy)
        |> toggleCell (x, y)


let movePlayer (key: string) (grid: Grid) (player: Player) : Player =
    let bound x = clamp x 0 grid.size

    if not (key.EndsWith "Arrow") then
        player
    else
        let direction = key.Substring(0, key.Length - 5)

        match direction with
        | "Up" -> { player with y = bound (player.y - 1) }
        | "Down" -> { player with y = bound (player.y + 1) }
        | "Left" -> { player with x = bound (player.x - 1) }
        | "Right" -> { player with x = bound (player.x + 1) }
        | _ -> player

let rec game (grid: Grid) (player: Player) =
    clearScreen ()
    displayGrid grid player

    let key = (Console.ReadKey true).Key.ToString()
    Threading.Thread.Sleep 50
    game (affectGrid key grid player) (movePlayer key grid player)

let rec promptInt (text: string) : int =
    printf "%s" text
    match Int32.TryParse(Console.ReadLine()) with
        | true, value -> value
        | false, _ ->
            printfn "A valid integer"
            promptInt text

let size: int = promptInt "Grid Size: "
let grid: Grid = generateGrid size
let player: Player = { x = 0; y = 0 }
Console.CursorVisible <- false
game grid player
