#load "common.fs"

module part1 =
    let solve (count: int) (input: list<string>) =
        let cups =
            input.Head
            |> Seq.map (fun x -> int (x.ToString()))
            |> Seq.toList

        let find3 (si: int) (cups: list<int>) =
            let n = cups.Length
            cups.[si + 1..si + 3] @ cups.[..(si + 3 - n)]

        let destination (v: int) (l: list<int>) =
            let sl = List.sort l
            let iv = List.findIndex (fun x -> x = v) sl
            if iv - 1 < 0 then
                let v = sl.[sl.Length - 1]
                List.findIndex (fun x -> x = v) l
            else
                let v = sl.[iv - 1]
                List.findIndex (fun x -> x = v) l

        seq { 1 .. count }
        |> Seq.fold (fun (si, cups: list<int>) _ ->
            let next3 = find3 si cups
            let but3 = List.except next3 cups
            let sv = cups.[si]
            let di = destination sv but3
            let cups = but3.[0..di] @ next3 @ but3.[di + 1..]

            let ni =
                ((List.findIndex (fun x -> x = sv) cups) + 1) % cups.Length

            (ni, cups)) (0, cups)
        |> snd

module part2 =
    let solve (count: int) (input: list<string>) = ignore
// part1 is kinda inefficieunt implementation. If the impl is made to use
// a efficient DS, it is possible. F# uses a singly linked list, but something better
// should work
// Also the patterns repeat, so there wouldn't be a need to enumerate the 10M

let input = Common.readIn
input |> part1.solve 100 |> Common.writeOut
