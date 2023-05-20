namespace ParserAnalyzer

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
