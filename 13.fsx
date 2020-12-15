#load "common.fs"

open System

module part1 =
    let solve (input: list<string>) =
        let earliest = int (input.Head)

        let ids =
            (List.last input).Split(",")
            |> Array.filter (fun x -> x <> "x")
            |> Array.map int
            |> Array.toSeq

        let min = 
            ids
            |> Seq.map (fun x -> (x - (earliest % x), x))
            |> Seq.minBy fst
        (fst min) * (snd min)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
