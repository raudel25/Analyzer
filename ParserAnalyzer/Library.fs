﻿namespace ParserAnalyzer

open System

module Parser =
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let run parser input =
        let (Parser innerFn) = parser

        innerFn input

    let pChar charToMath =
        let innerFn str =
            if String.IsNullOrEmpty str then

                Failure "No more input"
            else
                let first = str[0]

                if first = charToMath then
                    let remaining = str[1..]

                    Success(charToMath, remaining)
                else
                    let msg = $"Excepting '%c{charToMath}'. Got '%c{first}'"

                    Failure msg

        Parser innerFn

    let andThen parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Success (value1, remaining1) ->
                let result2 = run parser2 remaining1

                match result2 with
                | Success (value2, remaining2) ->
                    let newValue = (value1, value2)

                    Success(newValue, remaining2)

                | Failure err -> Failure err

            | Failure err -> Failure err

        Parser innerFn

    let (.>>.) = andThen

    let orElse parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Success _ -> result1
            | Failure _ ->
                let result2 = run parser2 input

                result2

        Parser innerFn

    let (<|>) = orElse

    let choice listOfParsers = List.reduce (<|>) listOfParsers

    let anyOf listOfChars = listOfChars |> List.map pChar |> choice
