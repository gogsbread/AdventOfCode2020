#load "common.fs"

let moveXY x y r c w = (r + x, (c + y) % w)

module part1 =
    let solve (input: string list) =
        let h = input.Length
        let w = input.Head.Length

        let r, c = (0, 0)
        seq { 1 .. h - 1 }
        |> Seq.mapFold (fun (r, c) _ ->
            let x, y = moveXY r c 1 3 w
            let c = if input.[x].[y] = '#' then 1 else 0
            (c, (x, y))) (r, c)
        |> fst
        |> Seq.sum

module part2 =
    let solve (input: string list) =
        let h = input.Length
        let w = input.Head.Length

        let r, c = (0, 0)
        seq {
            (moveXY 1 1, 1)
            (moveXY 1 3, 1)
            (moveXY 1 5, 1)
            (moveXY 1 7, 1)
            (moveXY 2 1, 2)
        }
        |> Seq.fold (fun p (mover, step) ->
            let s =
                seq { 1 .. step ..  h - 1 }
                |> Seq.mapFold (fun (r, c) _ ->
                    let x, y = mover r c w
                    let c = if input.[x].[y] = '#' then 1 else 0
                    (c, (x, y))) (r, c)
                |> fst
                |> Seq.sum

            (int64 s) * p) 1L

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut