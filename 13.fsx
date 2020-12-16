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

module part2 =
    let solve (input: list<string>) =
        // Sample code taken from rosetta code for Chines Remainder 
        // Some problem; reverted to using python to solve
        let rec sieve cs x N =
            match cs with
            | [] -> Some(x)
            | (a,n)::rest ->
                let arrProgress = Seq.unfold (fun x -> Some(x, x+N)) x
                let firstXmodNequalA = Seq.tryFind (fun x -> a = x % n)
                match firstXmodNequalA (Seq.take n arrProgress) with
                | None -> None
                | Some(x) -> sieve rest x (N*n)

        let CD congruences =
            let cs =
                congruences
                |> List.map (fun (a,n) -> (a % n, n))
                |> List.sortBy (snd>>(~-)) 
            let an = List.head cs
            sieve (List.tail cs) (fst an) (snd an)

        let ids =
            (List.last input).Split(",")
            |> Array.mapi (fun i s -> if s <> "x" then Some (i, int s) else None)
            |> Array.choose id
            |> Array.map (fun (i, x) -> ((x - (i % x)) % x, x))
            |> Array.toList

        (CD ids).Value

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut