#load "common.fs"

type Ins = { Op: string; Arg: int }

let parse (s: string) =
    let ins = s.Split(' ')
    { Op = ins.[0]; Arg = (int ins.[1]) }

module part1 =
    let solve (input: List<string>) =
        let insts = input |> List.map parse

        let rec compute (pc: int) (acc: int) (executedIns: Set<int>) =
            if executedIns.Contains pc then
                acc
            else
                let { Op = o; Arg = a } = insts.[pc]
                match o with
                | "acc" -> compute (pc + 1) (acc + a) (executedIns.Add pc)
                | "jmp" -> compute (pc + a) acc (executedIns.Add pc)
                | "nop" -> compute (pc + 1) acc (executedIns.Add pc)
                | _ -> invalidArg o "Unknown op"

        compute 0 0 Set.empty

module part2 =
    let solve (input: List<string>) =
        let insts = input |> List.map parse

        let rec compute (pc: int) (acc: int) (executedIns: Set<int>) (insts: List<Ins>) =
            if pc = insts.Length then
                (true, acc)
            elif pc > insts.Length || executedIns.Contains pc then
                (false, acc)
            else
                let { Op = o; Arg = a } = insts.[pc]
                match o with
                | "acc" -> compute (pc + 1) (acc + a) (executedIns.Add pc) insts
                | "jmp" -> compute (pc + a) acc (executedIns.Add pc) insts
                | "nop" -> compute (pc + 1) acc (executedIns.Add pc) insts
                | _ -> invalidArg o "Unknown op"

        insts
        |> List.indexed
        |> List.filter (fun x -> (snd x).Op = "jmp")
        |> List.map(fst >> (fun i -> insts.[0..i - 1] @ [ { Op = "nop"; Arg = -1 } ] @ insts.[i + 1..]))
        |> List.pick (fun l ->
            let success, acc = compute 0 0 Set.empty l
            if success then Some acc else None)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
