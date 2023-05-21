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

    let mapP f parser =
        let innerFn input =
            let result = run parser input

            match result with
            | Success (value, remaining) ->
                let newValue = f value

                Success(newValue, remaining)

            | Failure err -> Failure err

        Parser innerFn

    let (|>>) x f = mapP f x

    let returnP x =
        let innerFn input = Success(x, input)

        Parser innerFn

    let applyP fP fx = (fP .>>. fx) |>> (fun (f, x) -> f x)

    let (<*>) = applyP

    let lift2 f xP yP = returnP f <*> xP <*> yP

    let rec sequence parserList =

        let cons head tail = head :: tail

        match parserList with
        | [] -> returnP []
        | head :: tail -> lift2 cons head (sequence tail)

    let charListToStr charList = charList |> List.toArray |> String

    let pString str =
        str |> List.ofSeq |> List.map pChar |> sequence |>> charListToStr

    let rec parserZeroOrMore parser input =
        let firstResult = run parser input

        match firstResult with
        | Success (firstValue, firstRemaining) ->
            let secondValue, secondRemaining = parserZeroOrMore parser firstRemaining

            (firstValue :: secondValue, secondRemaining)

        | Failure _ -> ([], input)

    let many parser =
        let innerFn input =
            let result = parserZeroOrMore parser input
            Success(result)

        Parser innerFn

    let many1 parser =
        let innerFn input =
            let result = parserZeroOrMore parser input

            match result with
            | [], _ -> Failure "errr"
            | _ -> Success(result)

        Parser innerFn


    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let pInt =
        let intToStr (sign, digitsList) =
            let i = digitsList |> List.toArray |> String |> int

            match sign with
            | Some _ -> -i
            | None -> i

        let digit = anyOf [ '0' .. '9' ]

        let digits = many1 digit

        opt (pChar '-') .>>. digits |>> intToStr

    let (.>>) p1 p2 = p1 .>>. p2 |>> fst

    let (>>.) p1 p2 = p1 .>>. p2 |>> snd

    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP |>> fun (p, pList) -> p :: pList

    let sepBy p sep = sepBy1 p sep <|> returnP []

    let bindP f p =
        let innerFn input =
            let result1 = run p input

            match result1 with
            | Failure err ->

                Failure err
            | Success (value1, remainingInput) ->

                let p2 = f value1

                run p2 remainingInput

        Parser innerFn
