#load "common.fs"
#nowarn "0025"

open System

module part1 =
    let solve (input: list<string>) =
        let p1 =
            input.Tail
            |> List.takeWhile (String.IsNullOrWhiteSpace >> not)
            |> List.map int

        let p2 =
            (List.skipWhile (fun (s: string) -> not (s.Contains("Player 2:"))) input)
                .Tail
            |> List.map int

        let rec play (p1: list<int>) (p2: list<int>) =
            match (p1, p2) with
            | ([], _) -> p2
            | (_, []) -> p1
            | (h1 :: r1, h2 :: r2) -> if h1 > h2 then play (r1 @ @ [ h1 ] [ h2 ]) r2 else play r1 (r2 @ @ [ h2 ] [ h1 ])

        play p1 p2
        |> List.rev
        |> List.indexed
        |> List.sumBy (fun (i, x) -> (i + 1) * x)

module part2 =
    let solve (input: list<string>) =
        let p1 =
            input.Tail
            |> List.takeWhile (String.IsNullOrWhiteSpace >> not)
            |> List.map int

        let p2 =
            (List.skipWhile (fun (s: string) -> not (s.Contains("Player 2:"))) input)
                .Tail
            |> List.map int

        let rec play (p1: list<int>) (p2: list<int>) (game: int) (gameStates: Map<int, Set<int>>) =
            match (p1, p2) with
            | ([], _) -> (None, Some p2, Map.add game Set.empty gameStates)
            | (_, []) -> (Some p1, None, Map.add game Set.empty gameStates)
            | (h1 :: r1, h2 :: r2) ->
                if gameStates.[game].Contains((p1, p2).GetHashCode()) then
                    (Some [], None, Map.add game Set.empty gameStates)
                elif r1.Length >= h1 && r2.Length >= h2 then
                    match play r1.[..h1 - 1] r2.[..h2 - 1] (game + 1) (Map.add (game + 1) Set.empty gameStates) with
                    | (Some _, None, st) ->
                        play (r1 @ [ h1; h2 ]) r2 game (Map.add game (st.[game].Add((p1, p2).GetHashCode())) st)
                    | (None, Some _, st) ->
                        play r1 (r2 @ [ h2; h1 ]) game (Map.add game (st.[game].Add((p1, p2).GetHashCode())) st)
                    | _ -> invalidOp "Someone should be winner"
                elif h1 > h2 then
                    play
                        (r1 @ [ h1; h2 ])
                        r2
                        game
                        (Map.add game (gameStates.[game].Add((p1, p2).GetHashCode())) gameStates)
                else
                    play
                        r1
                        (r2 @ [ h2; h1 ])
                        game
                        (Map.add game (gameStates.[game].Add((p1, p2).GetHashCode())) gameStates)

        let (_, Some w2, _) =
            play p1 p2 0 (Map.empty |> Map.add 0 Set.empty)

        w2
        |> List.rev
        |> List.indexed
        |> List.sumBy (fun (i, x) -> (i + 1) * x)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
