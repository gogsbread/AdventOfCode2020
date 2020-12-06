#load "common.fs"

open System

type Part =
    | U
    | L

let rec id s e (space: Part list) =
    let m = (s + e) / 2
    match space with
    | [] -> m
    | U :: rest -> id s m rest
    | L :: rest -> id (m + 1) e rest

let toSection (input: string) =
    input.ToCharArray()
    |> Seq.map(fun x -> 
        match x with
        | 'F' -> U
        | 'B' -> L
        | 'L' -> U
        | 'R' -> L
        | _ -> raise (InvalidOperationException("Input invalid"))
    )

let sections (s: string) = 
    let r = s.[0 .. 6]
    let c = s.[7 ..]
    (toSection r, toSection c)

let seatId (row: seq<Part>, col: seq<Part>) =
    let toId (x: Part list) = id 0 ((1 <<< x.Length) - 1) x

    let rowId = (Seq.toList >> toId) row
    let colId = (Seq.toList >> toId) col

    let seatId = (rowId * 8) + colId
    seatId

module part1 =
    let solve (input: string list) =
        input
        |> List.map (sections >> seatId)
        |> List.max

module part2 = 
    let solve (input: string list) =
        let rec missingSeats (expected: int) (sortedSeats: int list) =
            match sortedSeats with
            | [] -> []
            | actual::rest ->
                if expected <> actual then
                    expected::(missingSeats (actual + 1) rest)
                else
                    missingSeats (expected + 1) rest

        let seats =
            input
            |> List.map (sections >> seatId)
            |> List.sort
        (missingSeats seats.Head seats).Head


let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
// input |> List.map part1.sections |> List.map part1. seatId |> Common.writeOut
// fst input |> toSection |> Seq.toList |> id 0 
