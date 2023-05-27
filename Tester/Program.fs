open ParserAnalyzer.JSON
open ParserAnalyzer.Analyzer

run jBool "true" |> printResult
// Success: JBool true

run jBool "false" |> printResult
// Success: JBool false

run jBool "truX" |> printResult

run jString "\"\"" |> printResult
  // Success (JString "")
run jString "\"a\""|> printResult
  // Success (JString "a")
run jString "\"ab\""|> printResult
  // Success (JString "ab")
run jString "\"ab\\tde\""|> printResult
  // Success (JString "ab\tde")
run jString "\"ab\\u263Ade\""|> printResult
  // Success (JString "ab☺de")