// For more information see https://aka.ms/fsharp-console-apps

open ParserAnalyzer
open ParserAnalyzer.Parser

let parseA = Parser.pChar 'A'
let inputABC = "ABC"
let parseResult = Parser.run parseA inputABC
let inputZBC = "ZBC"
// let run = Parser.run parseA inputZBC

let parseLowercase = anyOf [ 'a' .. 'z' ]

let x = Parser.run parseLowercase "aBC" // Success ('a', "BC")
let y = Parser.run parseLowercase "ABC" // Failure "Expecting 'z'. Got 'A'"

let parsers = [ pChar 'A'; pChar 'B'; pChar 'C' ]
let combined = sequence parsers

let q = run combined "ABCD"

let parseABC = pString "ABC"

let z = run parseABC "ABCDE" // Success ("ABC", "DE")
let t = run parseABC "A|CDE" // Failure "Expecting 'B'. Got '|'"
let v = run parseABC "AB|DE" // Failure "Expecting 'C'. Got '|'"

let digit = anyOf [ '0' .. '9' ]

// define parser for one or more digits
let digits = pInt

let p = run digits "-45BC" // Success (['1'], "ABC")

// let digit = anyOf [ '0' .. '9' ]

// use .>> below
let digitThenSemicolon = digit .>> opt (pChar ';')

let aa = run digitThenSemicolon "1;" // Success ('1', "")
let bb = run digitThenSemicolon "1" // Success ('1', "")

let charFunc =
    match bb with
    | Success (a, b) -> printfn $"success %c{a} %s{b}"
    | Failure e -> printf $"err: %s{e}"
