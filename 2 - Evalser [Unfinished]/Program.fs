open Tokenizer
open Expression
open System

let rec prompt() =
    printf "> "
    let input = Console.ReadLine().Trim()
    printfn "%A" (tokenize input)
    if input <> "#exit" then prompt()


prompt()
