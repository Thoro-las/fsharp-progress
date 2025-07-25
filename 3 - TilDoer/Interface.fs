module Interface

open System

let inline pause() = 
    printfn "\n\n(Q) to quit"
    while Console.ReadKey(true).Key <> ConsoleKey.Q do ()

let private prompt (prefix: string) : string = 
    printf "%s " prefix
    Console.ReadLine().Trim()

module Inputs =
    type private InputKind =
        | Text
        | Number
        | Index of int

    type InputValue =
        | Text of string
        | Number of double
        | Index of int

    type InputField = private {
        name: string;
        kind: InputKind;
        validate: InputValue -> bool * string;
    }

    let NumberField (name: string) = {
        name = name;
        kind = InputKind.Number;
        validate = fun _ -> true, "";
    }

    let TextField (name: string) = {
        name = name;
        kind = InputKind.Text;
        validate = fun _ -> true, "";
    }

    let IndexField (name: string) (range: int) = {
        name = name;
        kind = InputKind.Index range;
        validate = fun value ->
            match value with
                | Index n -> 
                    if n >= 1 && n <= range + 1 then true, ""
                    else false, "out of range"
                | _ -> false, "invalid index"
    }

    let validate (validate: InputValue -> bool * string) (field: InputField) =
        {field with validate = validate}

    let rec private promptField (error: string) (field: InputField) : InputValue =
        let promptText = if error <> ""
                         then $"Warning: {error}\n|> {field.name}:"
                         else $"|> {field.name}:"
        let input = prompt promptText
        let parsedInput =
            match field.kind with
                | InputKind.Text -> Text input
                | InputKind.Number ->
                    let valid, value = Double.TryParse input
                    match valid with
                        | false -> promptField "invalid number" field
                        | true -> Number value
                | InputKind.Index _ ->
                    let valid, value = Int32.TryParse input
                    match valid with
                        | false -> promptField "invalid index" field
                        | true -> Index value

        let valid, message = field.validate parsedInput
        match valid with
            | false -> promptField message field
            | true -> parsedInput

    type InputForm = private {
        title: string;
        inputs: InputField list;
    }
    
    let createForm (title: string) : InputForm = {
        title = title;
        inputs = [];
    }

    let (+>) (form: InputForm) (field: InputField) : InputForm =
        let hasName = not (form.inputs 
                    |> List.filter (fun x -> x.name = field.name)
                    |> List.isEmpty)

        match hasName with
            | true -> form
            | false -> {form with inputs = form.inputs @ [field]}

    let promptForm (form: InputForm) : (string * InputValue) list =
        form.inputs |> List.map (fun field -> field.name, promptField "" field)

module Menus =
    open Inputs

    type MenuAction<'T> = private {
        name: string;
        description: string;
        operation: 'T -> 'T * string;
    }

    type Menu<'T> = private { 
        name: string;
        actions: MenuAction<'T> list
    }

    let getActionByName (name: string) (menu: Menu<'T>) =
        menu.actions |> List.filter (fun action -> action.name = name) |> List.tryHead

    let rec private promptMenu (note: string) (text: string) (menu: Menu<'T>) : MenuAction<'T> =
        Console.Clear()
        Console.WriteLine $"{text}\n"
        menu.actions |> List.iter (fun action -> printfn "|-> %s" action.name)

        Console.WriteLine $"\n{note}"
        let input = prompt "| :"
        let found = menu |> getActionByName input

        match found with
            | Some action -> action
            | None -> 
                if input = "" then promptMenu "" text menu
                else promptMenu $"Action ({input}) not found." text menu

    let rec private showMenu' (elementWithNote: 'T * string) (descriptor: 'T -> string) (menu: Menu<'T>) =
        let element, note = elementWithNote
        let action = menu |> promptMenu note (descriptor element)
        let operatedElement = action.operation element

        menu |> showMenu' operatedElement descriptor

    let rec showMenu (element: 'T) (descriptor: 'T -> string) (menu: Menu<'T>) =
        showMenu' (element, "") descriptor menu

    let createMenu<'T> (name: string) : Menu<'T> = { name = name; actions = [] }

    let (++) (menu: Menu<'T>) (action: MenuAction<'T>) : Menu<'T> =
        if (getActionByName action.name menu).IsSome then menu
        else {menu with actions = menu.actions @ [action]}

    let MenuItem<'T> (name: string) (description: string) 
            (action: 'T -> 'T * string) : MenuAction<'T> = {
                name = name;
                description = description;
                operation = action;
            }

    let getMenuItems (menu: Menu<'T>) : MenuAction<'T> list =
        menu.actions

    let addHelpItem (menu: Menu<'T>) : Menu<'T> =
        menu ++ (MenuItem "help" "shows this help list"
            <| fun (element: 'T) ->
                let values =
                    createForm "help action"
                        +> TextField "action"
                    |> promptForm

                match values with
                    | ["action", Text action] ->
                        let menuitem =
                            menu.actions |> List.map (fun x -> x, x.name) 
                            |> List.filter (fun (_, name) -> name = action)
                            |> List.tryHead

                        match menuitem with
                            | None -> element, $"action ({action}) does not exist"
                            | Some (item, name) -> element, $"{name}: {item.description}"
                    | _ -> element, "could not find help")

