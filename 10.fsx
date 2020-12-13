#load "common.fs"

open System.Collections.Generic

module part1 =
    let solve (input: string list) =
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
    let solve (input: string list) =
        let x = input |> List.map int |> List.sort
        let adapters = [ 0 ] @ x @ [ List.max x + 3 ]

        let dp = Dictionary<int, int64>([ KeyValuePair(adapters.[0], 1L) ])

        for i in [ 1 .. adapters.Length - 1 ] do
            let v =
                List.sum [ for j in [ 1 .. 3 ] ->
                               if (i - j >= 0)
                                  && (adapters.[i] - adapters.[i - j] <= 3) then
                                   dp.[adapters.[i - j]]
                               else
                                   0L ]

            dp.Add(adapters.[i], v)
        dp.[adapters |> List.last]

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
