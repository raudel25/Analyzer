// For more information see https://aka.ms/fsharp-console-apps

open ParserAnalyzer
open ParserAnalyzer.Parser

let parseA = Parser.pChar 'A'
let inputABC = "ABC"
let parseResult = Parser.run parseA inputABC
let inputZBC = "ZBC"
let run = Parser.run parseA inputZBC

let parseLowercase = anyOf [ 'a' .. 'z' ]

let x = Parser.run parseLowercase "aBC" // Success ('a', "BC")
let y = Parser.run parseLowercase "ABC" // Failure "Expecting 'z'. Got 'A'"

let charFunc =
    match y with
    | Success (a, b) -> printfn $"success %c{a} %s{b}"
    | Failure _ -> printf "err"
