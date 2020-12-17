#load "common.fs"

open System.Text.RegularExpressions

type Mask = string

type Ins =
    | Mask of Mask
    | Mem of (int64 * int64)

let parse (s: string) =
    let maskMatch = Regex.Match(s, @"mask = (\w+)")
    let memMatch = Regex.Match(s, @"mem\[(\d+)\] = (\d+)")
    if maskMatch.Success then Mask(maskMatch.Groups.[1].Value)
    elif memMatch.Success then Mem(int64 memMatch.Groups.[1].Value, int64 memMatch.Groups.[2].Value)
    else invalidArg "s" "Invalid input"

module part1 =
    type ChipProgram =
        { BitMask: (int64 * int64)
          Memory: Map<int64, int64> }

    let eraseMask (m: string) =
        m
        |> Seq.fold (fun em c ->
            (em <<< 1) ||| (if c = 'X' then 1L else 0L)) 0L

    let mask (m: string) =
        m
        |> Seq.fold (fun em c ->
            (em <<< 1) ||| (if c = 'X' then 0L else int64 (string c))) 0L

    let solve (input: list<string>) =
        let output =
            input
            |> List.map parse
            |> List.fold (fun prgm ins ->
                match ins with
                | Mask m ->
                    { BitMask = (eraseMask m, mask m)
                      Memory = prgm.Memory }
                | Mem m ->
                    let loc, value = m
                    let eraseMask, mask = prgm.BitMask
                    let masked = (value &&& eraseMask) ||| mask
                    { BitMask = prgm.BitMask
                      Memory = Map.change loc (fun _ -> Some masked) prgm.Memory }
               ){
                  BitMask = (eraseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
                  Memory = Map.empty 
               }

        Map.toSeq output.Memory |> Seq.sumBy snd

module part2 =
    type ChipProgram =
        { BitMask: Mask
          Memory: Map<int64, int64> }

    let rec allAddress (loc: int64) (mask: Mask) =
        let maskedL =
            mask
            |> Seq.indexed
            |> Seq.fold (fun l (i, c) ->
                match c with
                | '1' -> l ||| (1L <<< (35 - i))
                | _ -> l) loc

        mask
        |> Seq.indexed
        |> Seq.fold (fun acc (i, c) ->
            match c with
            | 'X' ->
                // set 0 & 1
                let ii = 35 - i
                (acc |> List.map (fun x -> x &&& ~~~(1L <<< ii)))
                @ (acc |> List.map (fun x -> x ||| (1L <<< ii)))
            | _ -> acc) [ maskedL ]

    let solve (input: list<string>) =
        let output =
            input
            |> List.map parse
            |> List.fold (fun prgm ins ->
                match ins with
                | Mask m -> { BitMask = m; Memory = prgm.Memory }
                | Mem m ->
                    let loc, value = m
                    let addrs = allAddress loc prgm.BitMask

                    let updatedMemory =
                        addrs
                        |> List.fold (fun mem addr -> Map.add addr value mem) prgm.Memory

                    { BitMask = prgm.BitMask
                      Memory = updatedMemory }
              ) {
                  BitMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                  Memory = Map.empty 
                }

        Map.toSeq output.Memory |> Seq.sumBy snd

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
