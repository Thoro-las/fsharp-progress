module Tokenizer

open System
open System.Text.RegularExpressions

type TokenKind =
    | Value
    | Identifier
    | Operator
    | Command
    | Parenthesis
    | Unknown
    | EOF

type Token = {
    word: string;
    kind: TokenKind;
    value: obj option
}

let private (|Digit|_|) c = if Char.IsDigit c then Some c else None
let private (|Letter|_|) c = if Char.IsLetter c then Some c else None
let private (|Whitespace|_|) c = if Char.IsWhiteSpace c then Some c else None
let private (|Operator|_|) c = if [|'+'; '-'; '/'; '*'; '^'; '$'|] |> Array.contains c then Some c else None
let private (|Parenthesis|_|) c = if [|'('; ')'|] |> Array.contains c then Some c else None

let expectedToken (c: char) : TokenKind list =
    match c with
        | Digit _ -> [Value]
        | Letter _ -> [Identifier; Command]
        | Parenthesis _ -> [Parenthesis]
        | Operator _ -> [Operator]
        | _ -> [Unknown]


type TokenTree =
    | Leaf of Token
    | Branch of Token * (Token list)

