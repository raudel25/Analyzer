open ParserAnalyzer.JSON
open ParserAnalyzer.Analyzer

run jBool "true" |> printResult
// Success: JBool true

run jBool "false" |> printResult
// Success: JBool false

run jBool "truX" |> printResult

run jString "\"\"" |> printResult
// Success (JString "")
run jString "\"a\"" |> printResult
// Success (JString "a")
run jString "\"ab\"" |> printResult
// Success (JString "ab")
run jString "\"ab\\tde\"" |> printResult
// Success (JString "ab\tde")
run jString "\"ab\\u263Ade\"" |> printResult
// Success (JString "ab☺de")

run jNumber "123" |> printResult// JNumber 123.0
run jNumber "-123" |> printResult// JNumber -123.0

run pFloat "-123." |> printResult

run pFloat "00.4" |> printResult
// printf "$"
