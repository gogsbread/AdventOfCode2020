#load "common.fs"

module part1 =
    let solve (input: List<string>) =
            let diffs =
                input
                |> List.map int
                |> List.sort
                |> List.mapFold (fun acc j -> (j - acc), j) 0
                |> fst
                |> List.countBy id
                |> Map.ofList
            diffs.[1] * (diffs.[3] + 1)

module part2 =
    let rec arrangements (adapters: List<int>) =
        match adapters with
        | f::s::t::fr::_ -> 
            if fr - f = 3 then
                (1 <<< 2) * arrangements (List.skip 3 adapters)
            elif t - f = 2 then
                (1 <<< 1) * arrangements (List.skip 2 adapters)
            else
                arrangements (List.skip 1 adapters)
        | f::s::t::_ -> 
            if t - f <= 3 then 2 else 1
        | _ ->
            1

    let solve (input: List<string>) =
        let x = 
            input
            |> List.map int
            |> List.sort
        let adapters = [0] @ x @ [(List.max x) + 3]
        arrangements adapters

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut