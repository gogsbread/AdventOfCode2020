#load "common.fs"

module part =
    let solve (limit: int) (input: string) =
        let numbers = input.Split(",")

        let startingNumbers =
            numbers
            |> Array.map int
            |> Array.indexed
            |> Array.fold (fun m (i, x) -> Map.add x (-1, (i + 1)) m) Map.empty

        let speak (n: int) (turn: int) (st: Map<int, int * int>) =
            (n,
             Map.change n (fun v ->
                 match v with
                 | Some (_, pp) -> Some(pp, turn)
                 | None -> Some(-1, turn)) st)

        let last = (int (Array.last numbers))
        seq { (numbers.Length + 1) .. limit }
        |> Seq.fold (fun (last: int, st: Map<int, int * int>) turn ->
            let (pp, p) = st.[last]
            if pp = -1 then speak 0 turn st else speak (p - pp) turn st) (last, startingNumbers)
        |> fst

let input = Common.readIn
input.Head |> part.solve 2020 |> Common.writeOut
input.Head |> part.solve 30000000 |> Common.writeOut
