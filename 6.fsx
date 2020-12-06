#load "common.fs"

open System

module part1 =
    let rec parse (group: char Set) (responses: string list) =
        match responses with
        | [] -> [ group ]
        | resp :: rest ->
            match String.IsNullOrWhiteSpace(resp) with
            | true -> group :: (parse Set.empty rest)
            | false ->
                let answers = Set.ofArray (resp.ToCharArray())
                parse (Set.union group answers) rest

    let solve (input: string list) =
        input
        |> parse Set.empty
        |> List.sumBy (fun x -> x.Count)

module part2 =
    // Solve by using parsing the first one and threading that state as `group` input set
    let rec parse (group: char Set) (responses: string list) =
        match responses with
        | [] -> [ group ]
        | resp :: next :: rest ->
            match String.IsNullOrWhiteSpace(resp) with
            | true ->
                group
                :: (parse (Set.ofArray (next.ToCharArray())) rest)
            | false ->
                let answers = Set.ofArray (resp.ToCharArray())
                parse (Set.intersect group answers) (next :: rest)
        | resp :: rest ->
            let answers = Set.ofArray (resp.ToCharArray())
            parse (Set.intersect group answers) rest

    let solve (input: string list) =
        input.Tail
        |> parse (Set.ofArray (input.Head.ToCharArray()))
        |> List.sumBy (fun x -> x.Count)

let input = Common.readIn
input |> part1.solve |> Common.writeOut

input |> part2.solve |> Common.writeOut
// input |> part1.parse Set.empty |> List.iter (printfn "%A")
