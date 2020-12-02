#load "common.fs"

module part1 =
    let solve input =
        input
        |> Seq.fold (fun (product, elements: int Set) x ->
            if elements.Contains(2020 - x) then (product * x * (2020 - x), elements) else (product, elements.Add x))
               (1, Set.empty)
        |> fst

module part2 =
    let solve input =
        let a = 
            input
            |> Seq.toArray

        let n = a.Length
        seq {
            for i in 0 .. (n - 1) do
                for j in (i + 1) .. (n - 1) do
                    for k in (j + 1) .. (n - 1) -> 
                        (a.[i], a.[j], a.[k])
        }
        |> Seq.pick (fun (x, y, z) ->
            match (x + y + z) with
            | 2020 -> Some (x * y * z)
            | _ -> None)

let input = Common.readIn

input
|> Seq.map int
|> part1.solve
|> Common.writeOut

input
|> Seq.map int
|> part2.solve
|> Common.writeOut
