// For more information see https://aka.ms/fsharp-console-apps

open ParserAnalyzer
let parseA = Parser.pChar 'A'
let inputABC = "ABC"
let parseResult = Parser.run parseA inputABC
let inputZBC = "ZBC"
let run = Parser.run parseA inputZBC