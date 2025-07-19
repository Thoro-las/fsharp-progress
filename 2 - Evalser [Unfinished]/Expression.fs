module Expression

open Utility

type EvalResult = Result<int, string>

type Atom =
    | Val of int 
    | Var of char

let getValue (atom: Atom) : EvalResult =
    match atom with
        | Val num -> Ok num
        | Var _ -> Ok -1

type Operator = {
    arity: int;
    symbol: string;
    operation: ((EvalResult) list -> EvalResult)
}

let createBinaryOperation (operation: int * int -> int) : int list -> int =
    function | [a; b] -> operation (a, b) | _ -> 0

let createOperator (arity: int) (symbol: string) (operation: int list -> int) : Operator = {
        arity = arity;
        symbol = symbol;
        operation =
            fun (terms: EvalResult list) -> 
                let truncated = terms |> List.truncate arity
                let oks = getAllOk truncated
                match oks with
                    | None -> Error "Couldn't Evaluate"
                    | Some terms -> Ok (operation terms)
    }

type Expr =
    | Atom of Atom
    | Apply of Operator * (Expr list)


let rec evaluateExpression (expression: Expr) : EvalResult =
    match expression with
        | Atom atom -> getValue atom
        | Apply (operator, terms) ->
            if terms.Length <> operator.arity then Error "Not Enough Arguments"
            else operator.operation (List.map evaluateExpression terms)

let Mult: Operator = createOperator 2 "*" (createBinaryOperation (fun (x, y) -> x * y))
let Add: Operator = createOperator 2 "+" (createBinaryOperation (fun (x, y) -> x + y))
