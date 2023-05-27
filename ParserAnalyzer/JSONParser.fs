namespace ParserAnalyzer

open Analyzer
open System

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

module JSON =
    let (>>%) p x = p |>> (fun _ -> x)

    let jNull = pString "null" >>% JNull <?> "null"

    let jBool =
        let jTrue = pString "true" >>% JBool true
        let jFalse = pString "false" >>% JBool false

        jTrue <|> jFalse <?> "bool"

    let jUnEscapedChar =
        let label = "char"
        satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

    let jEscapedChar =
        [ ("\\\"", '\"')
          ("\\\\", '\\')
          ("\\/", '/')
          ("\\b", '\b')
          ("\\f", '\f')
          ("\\n", '\n')
          ("\\r", '\r')
          ("\\t", '\t') ]

        |> List.map (fun (toMatch, result) -> pString toMatch >>% result)
        |> choice
        <?> "escaped char"

    let jUnicodeChar =
        let backslash = pChar '\\'
        let uChar = pChar 'u'
        let hexDigit = anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])
        let fourHexDigits = hexDigit .>>. hexDigit .>>. hexDigit .>>. hexDigit

        let convertToChar (((h1, h2), h3), h4) =
            let str = $"%c{h1}%c{h2}%c{h3}%c{h4}"
            Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char

        backslash >>. uChar >>. fourHexDigits |>> convertToChar

    let quotedString =
        let quote = pChar '\"' <?> "quote"
        let jChar = jUnEscapedChar <|> jEscapedChar <|> jUnicodeChar

        quote >>. manyChars jChar .>> quote

    let jString = quotedString <?> "quoted string" |>> JString
