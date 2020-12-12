#load "common.fs"

module part1 =
    let isTwoPartSum (a: int64) (l: List<int64>) =
        List.allPairs l l
        |> List.tryPick (fun (x, y) -> if x + y = a then Some true else None)

    let rec weakXmas (n: int) (data: List<int64>) =
        let preamble = data |> List.take n
        let next = data |> List.skip n
        match isTwoPartSum next.Head preamble with
        | Some _ -> weakXmas n data.Tail
        | None -> next.Head

    let solve (n: int) (input: List<string>) =
        input |> List.map int64 |> weakXmas n

module part2 =
    let solve (n: int) (input: List<string>) =
        let weakX = part1.solve n input
        let data = input |> List.map int64

        let rec encWeak (s: int) (e: int) (rs: int64) =
            if rs = weakX then
                let min = List.min data.[s..e]
                let max = List.max data.[s..e]
                min + max
            elif rs < weakX then
                encWeak s (e + 1) (rs + data.[e + 1])
            else
                encWeak (s + 1) e (rs - data.[s])

        encWeak 0 0 data.[0]

let input = Common.readIn
// input |> part1.solve 5 |> Common.writeOut
input |> part1.solve 25 |> Common.writeOut
// input |> part2.solve 5 |> Common.writeOut
input |> part2.solve 25 |> Common.writeOut
