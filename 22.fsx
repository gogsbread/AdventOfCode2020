#load "common.fs"

open System

module part1 =
    let solve (input: list<string>) =
        let p1 =
            input.Tail
            |> List.takeWhile (String.IsNullOrWhiteSpace >> not)
            |> List.map int

        let p2 = 
            (List.skipWhile (fun (s: string) -> not (s.Contains("Player 2:"))) input).Tail
            |> List.map int

        let rec play (p1: list<int>) (p2: list<int>) =
            match (p1, p2) with
            | ([], _) -> p2
            | (_, []) -> p1
            | (h1::r1, h2::r2) ->
                if h1 > h2 then
                    play (r1 @ [h1] @ [h2]) r2
                else
                    play r1 (r2 @ [h2] @ [h1])

        play p1 p2
        |> List.rev
        |> List.indexed
        |> List.sumBy (fun (i, x) -> (i + 1) * x)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
