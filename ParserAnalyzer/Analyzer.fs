namespace ParserAnalyzer

open System

type ParserLabel = string
type ParserError = string

type ParserPosition =
    { currentLine: string
      line: int
      column: int }

type Position = { line: int; column: int }

type InputState = { lines: string[]; position: Position }

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'a> =
    { parseFn: InputState -> ParseResult<'a * InputState>
      label: ParserLabel }

module Input =
    let initialPos = { line = 0; column = 0 }

    let incrCol (pos: Position) = { pos with column = pos.column + 1 }

    let incrLine pos = { line = pos.line + 1; column = 0 }

    let fromStr str =
        if String.IsNullOrEmpty(str) then
            { lines = [||]; position = initialPos }
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            { lines = lines; position = initialPos }

    let currentLine input =
        let pos = input.position.line

        if pos < input.lines.Length then
            input.lines[pos]
        else
            "end of file"

    let nextChar input =
        let pos = input.position.line
        let colPos = input.position.column

        if pos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input

            if colPos < currentLine.Length then
                let char = currentLine[colPos]
                let newPos = incrCol input.position
                let newState = { input with position = newPos }
                newState, Some char
            else
                let char = '\n'
                let newPos = incrLine input.position
                let newState = { input with position = newPos }
                newState, Some char

    let parserPositionFromInputState (inputState: InputState) =
        { currentLine = currentLine inputState
          line = inputState.position.line
          column = inputState.position.column }

module Analyzer =
    let runOnInput parser input = parser.parseFn input

    let run parser inputStr =
        runOnInput parser (Input.fromStr inputStr)

    let printResult result =
        match result with
        | Success (value, _input) -> printfn $"%A{value}"
        | Failure (label, error, pos) ->
            let errorLine = pos.currentLine
            let colPos = pos.column
            let linePos = pos.line
            let failureCaret = sprintf "%*s^%s" colPos "" error
            printfn $"Line:%i{linePos} Col:%i{colPos} Error parsing %s{label}\n%s{errorLine}\n%s{failureCaret}"

    let setLabel parser newLabel =
        let newInnerFn input =
            let result = parser.parseFn input

            match result with
            | Success s -> Success s
            | Failure (_, err, pos) -> Failure(newLabel, err, pos)

        { parseFn = newInnerFn
          label = newLabel }

    let (<?>) = setLabel

    let getLabel parser = parser.label

    let satisfy predicate label =
        let innerFn input =
            let remainingInput, charOpt = Input.nextChar input

            match charOpt with
            | None ->
                let err = "No more input"
                let pos = Input.parserPositionFromInputState input

                Failure(label, err, pos)
            | Some first ->
                if predicate first then
                    Success(first, remainingInput)
                else
                    let err = $"Unexpected '%c{first}'"
                    let pos = Input.parserPositionFromInputState input

                    Failure(label, err, pos)

        { parseFn = innerFn; label = label }

    let pChar charToMatch =
        let predicate ch = (ch = charToMatch)
        let label = $"%c{charToMatch}"
        satisfy predicate label

    let andThen parser1 parser2 =
        let label = $"%s{getLabel parser1} andThen %s{getLabel parser2}"

        let innerFn input =
            let result1 = runOnInput parser1 input

            match result1 with
            | Success (value1, remaining1) ->
                let result2 = runOnInput parser2 remaining1

                match result2 with
                | Success (value2, remaining2) ->
                    let newValue = (value1, value2)

                    Success(newValue, remaining2)

                | Failure (fLabel, err, pos) -> Failure(fLabel, err, pos)

            | Failure (fLabel, err, pos) -> Failure(fLabel, err, pos)

        { parseFn = innerFn; label = label }

    let (.>>.) = andThen

    let orElse parser1 parser2 =
        let label = $"%s{getLabel parser1} orElse %s{getLabel parser2}"

        let innerFn input =
            let result1 = runOnInput parser1 input

            match result1 with
            | Success _ -> result1
            | Failure _ ->
                let result2 = runOnInput parser2 input

                result2

        { parseFn = innerFn; label = label }

    let (<|>) = orElse

    let bindP f p =
        let label = "unknown"

        let innerFn input =
            let result1 = runOnInput p input

            match result1 with
            | Failure (label, err, pos) ->

                Failure(label, err, pos)
            | Success (value1, remainingInput) ->

                let p2 = f value1

                runOnInput p2 remainingInput

        { parseFn = innerFn; label = label }

    let (>>=) p f = bindP f p

    let choice listOfParsers = List.reduce (<|>) listOfParsers

    let anyOf listOfChars =
        let label = $"any of %A{listOfChars}"
        listOfChars |> List.map pChar |> choice <?> label

    let mapP f parser =
        let label = "unknown"

        let innerFn input =
            let result = runOnInput parser input

            match result with
            | Success (value, remaining) ->
                let newValue = f value

                Success(newValue, remaining)

            | Failure (label, err, pos) -> Failure(label, err, pos)

        { parseFn = innerFn; label = label }

    let (|>>) x f = mapP f x

    let returnP x =
        let label = "unknown"

        let innerFn input = Success(x, input)

        { parseFn = innerFn; label = label }

    let applyP fP fx = (fP .>>. fx) |>> (fun (f, x) -> f x)

    let (<*>) = applyP

    let lift2 f xP yP = returnP f <*> xP <*> yP

    let rec sequence parserList =

        let cons head tail = head :: tail

        match parserList with
        | [] -> returnP []
        | head :: tail -> lift2 cons head (sequence tail)

    let charListToStr charList = String(List.toArray charList)

    let pString str =
        str |> List.ofSeq |> List.map pChar |> sequence |>> charListToStr <?> str

    let opt p =
        let label = $"opt %s{getLabel p}"
        let some = p |>> Some
        let none = returnP None
        (some <|> none) <?> label

    let rec parserZeroOrMore parser input =
        let firstResult = runOnInput parser input

        match firstResult with
        | Success (firstValue, firstRemaining) ->
            let secondValue, secondRemaining = parserZeroOrMore parser firstRemaining

            (firstValue :: secondValue, secondRemaining)

        | Failure _ -> ([], input)

    let many parser =
        let label = $"many %s{getLabel parser}"
        let innerFn input = Success(parserZeroOrMore parser input)

        { parseFn = innerFn; label = label }

    let many1 p =
        let label = $"many1 %s{getLabel p}"

        p >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail))) <?> label

    let manyChars cp = many cp |>> charListToStr

    let manyChars1 cp = many1 cp |>> charListToStr

    let (.>>) p1 p2 = p1 .>>. p2 |>> fst

    let (>>.) p1 p2 = p1 .>>. p2 |>> snd

    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP |>> fun (p, pList) -> p :: pList

    let sepBy p sep = sepBy1 p sep <|> returnP []

    let digitChar =
        let predicate = Char.IsDigit
        let label = "digit"
        satisfy predicate label

    let pInt =
        let label = "integer"

        let resultToInt (sign, digits) =
            let i = digits |> int

            match sign with
            | Some _ -> -i
            | None -> i

        let digits = manyChars1 digitChar

        opt (pChar '-') .>>. digits |> mapP resultToInt <?> label

    let pFloat =
        let label = "float"

        let resultToFloat (((sign, digits1), _), digits2) =
            let fl = $"%s{digits1}.%s{digits2}" |> float

            match sign with
            | Some _ -> -fl
            | None -> fl

        let digits = manyChars1 digitChar

        opt (pChar '-') .>>. digits .>>. pChar '.' .>>. digits |> mapP resultToFloat
        <?> label


    let whitespaceChar =
        let predicate = Char.IsWhiteSpace
        let label = "whitespace"
        satisfy predicate label

    let spaces = many whitespaceChar

    let spaces1 = many1 whitespaceChar
