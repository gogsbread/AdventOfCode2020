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
            |> List.fold (fun (tokens: list<Token>, operators: list<Token>) (token: Token) ->
                    match token with
                    | Operand t -> tokens @ [Operand t], operators
                    | Symbol s -> 
                        match s with
                        | OpenBrace -> tokens, Symbol s::operators
                        | CloseBrace -> 
                            let i = List.findIndex (fun op -> op = Symbol OpenBrace) operators
                            // printfn "%A %A" i operators
                            tokens @ (operators.[ .. i - 1]), operators.[ i + 1 .. ]
                        | _ -> 
                            let iOpenBrace = List.tryFindIndex (fun op -> op = Symbol OpenBrace) operators
                            match iOpenBrace with
                            | Some i -> tokens @ (operators.[ .. i - 1]), Symbol s::operators.[i .. ]
                            | None -> tokens @ operators, [Symbol s]
                ) (List.empty, List.empty)
        operands @ operators

    let solve (input: list<string>) =
        let expr = input.Head
        expr
        |> Seq.filter (fun c -> c <> ' ')
        |> Seq.map toToken
        |> Seq.toList
        |> toPostfix

let input = Common.readIn
input |> part1.solve |> Common.writeOut