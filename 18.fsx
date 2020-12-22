// add symbol to it

#load "common.fs"

type Symbol =
    | Add
    | Mul
    | OpenBrace
    | CloseBrace

type Operand = int

type Token =
    | Symbol of Symbol
    | Operand of Operand

let toToken (c: char): Token =
    match c with
    | '+' -> Symbol Add
    | '*' -> Symbol Mul
    | '(' -> Symbol OpenBrace
    | ')' -> Symbol CloseBrace
    | _ -> Operand (int (c.ToString()))

module part1 =
    let toPostfix (tokens: list<Token>): list<Token> =
        let operands, operators =
            tokens
            |> List.fold (fun (operands: list<Operand>, operators: list<Symbol>) (token: Token) ->
                    match token with
                    | Operand t -> t::operands, operators
                    | Symbol s -> operands, operators@[s]
                ) (List.empty, List.empty)
        (operands |> List.map Operand) @ (operators |> List.map Symbol)

    let solve (input: list<string>) =
        let expr = input.Head
        expr
        |> Seq.filter (fun c -> c <> ' ')
        |> Seq.map toToken
        |> Seq.toList
        |> toPostfix

let input = Common.readIn
input |> part1.solve |> Common.writeOut