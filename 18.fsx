#load "common.fs"
#nowarn "0025"

type Symbol =
    | Add
    | Mul
    | OpenBrace
    | CloseBrace

type Operand = int64

type Token =
    | Symbol of Symbol
    | Operand of Operand

let toToken (c: char): Token =
    match c with
    | '+' -> Symbol Add
    | '*' -> Symbol Mul
    | '(' -> Symbol OpenBrace
    | ')' -> Symbol CloseBrace
    | _ -> Operand(int64 (c.ToString()))

let tokens (s: seq<char>) =
    s
    |> Seq.fold (fun acc c -> acc @ [ toToken c ]) List.empty

let removeSpace (s: string) = s |> Seq.filter (fun c -> c <> ' ')

let eval (tokens: list<Token>) =
    tokens
    |> List.fold
        (fun stack token ->
            match token with
            | Operand opnd -> opnd :: stack
            | Symbol sym ->
                match sym with
                | Add ->
                    let (::) fst, snd :: rest = stack
                    (fst + snd) :: rest
                | Mul ->
                    let (::) fst, snd :: rest = stack
                    (fst * snd) :: rest
                | _ -> invalidArg "sym" "No other symbols should be here")
        List.empty

module part1 =
    let toPostfix (tokens: list<Token>): list<Token> =
        let operands, operators =
            tokens
            |> List.fold
                (fun (tokens: list<Token>, operators: list<Token>) (token: Token) ->
                    match token with
                    | Operand t -> tokens @ [ Operand t ], operators
                    | Symbol s ->
                        match s with
                        | OpenBrace -> tokens, Symbol s :: operators
                        | CloseBrace ->
                            let i =
                                List.findIndex (fun op -> op = Symbol OpenBrace) operators

                            tokens @ (operators.[..i - 1]), operators.[i + 1..]
                        | _ ->
                            let iOpenBrace =
                                List.tryFindIndex (fun op -> op = Symbol OpenBrace) operators

                            match iOpenBrace with
                            | Some i -> tokens @ (operators.[..i - 1]), Symbol s :: operators.[i..]
                            | None -> tokens @ operators, [ Symbol s ])
                (List.empty, List.empty)

        operands @ operators


    let solve (input: list<string>) =
        input
        |> Seq.map (removeSpace >> tokens)
        |> Seq.map toPostfix
        |> Seq.map eval
        |> Seq.map (fun oprnds -> oprnds.Head)
        |> Seq.sum

module part2 =
    let toPostfix (tokens: list<Token>): list<Token> =
        let operands, operators =
            tokens
            |> List.fold
                (fun (tokens: list<Token>, operators: list<Token>) (token: Token) ->
                    match token with
                    | Operand t -> tokens @ [ Operand t ], operators
                    | Symbol s ->
                        match s with
                        | OpenBrace -> tokens, Symbol s :: operators
                        | CloseBrace ->
                            let i =
                                List.findIndex (fun op -> op = Symbol OpenBrace) operators

                            tokens @ (operators.[..i - 1]), operators.[i + 1..]
                        | Add ->
                            let iNextPrecedence =
                                operators
                                |> List.indexed
                                |> List.tryPick
                                    (fun (i, op) ->
                                        match op with
                                        | Symbol Add -> None
                                        | _ -> Some(i - 1))

                            match iNextPrecedence with
                            | Some i -> tokens @ (operators.[..i]), Symbol s :: operators.[i + 1..]
                            | None -> tokens @ operators, [ Symbol s ]
                        | Mul ->
                            let iNextPrecedence =
                                operators
                                |> List.indexed
                                |> List.tryPick
                                    (fun (i, op) ->
                                        match op with
                                        | Symbol Add -> None
                                        | Symbol Mul -> None
                                        | _ -> Some(i - 1))

                            match iNextPrecedence with
                            | Some i -> tokens @ (operators.[..i]), Symbol s :: operators.[i + 1..]
                            | None -> tokens @ operators, [ Symbol s ])
                (List.empty, List.empty)

        operands @ operators


    let solve (input: list<string>) =
        input
        |> Seq.map (removeSpace >> tokens)
        |> Seq.map toPostfix
        |> Seq.map eval
        |> Seq.map (fun oprnds -> oprnds.Head)
        |> Seq.sum

let input = Common.readIn
// input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
