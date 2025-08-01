module SPD

open System
open System.IO

let writeToFile (date: DateTime) (filename: string) (text: string) =
    let path = sprintf "%s-%s.txt" (date.ToString "ddMMyy") filename
    File.WriteAllText(path, text)
