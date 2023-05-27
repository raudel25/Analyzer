namespace ParserAnalyzer

open Analyzer

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

module JSON =
    let jNull =
        pString "null" |>> (fun _ -> JNull) 
        <?> "null"
