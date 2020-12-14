#load "common.fs"

type Heading =
    | North = 0
    | South = 2
    | East = 1
    | West = 3

let direction (h: Heading) =
    match h with
    | Heading.North -> (-1, 0)
    | Heading.South -> (1, 0)
    | Heading.East -> (0, 1)
    | Heading.West -> (0, -1)
    | _ -> invalidArg "h" "Nan"

module part1 =
    let maneuver (x, y) (h: Heading) (command: string) =
        let action = command.[0]
        let arg = int (command.Substring 1)
        match action with
        | 'N' ->
            let dx, dy = direction Heading.North
            ((arg*dx + x, arg*dy + y), h)
        | 'S' ->
            let dx, dy = direction Heading.South
            ((arg*dx + x, arg*dy + y), h)
        | 'E' ->
            let dx, dy = direction Heading.East
            ((arg*dx + x, arg*dy + y), h)
        | 'W' ->
            let dx, dy = direction Heading.West
            ((arg*dx + x, arg*dy + y), h)
        | 'F' -> 
            let dx, dy = direction h
            ((arg*dx + x, arg*dy + y), h)
        | 'L' ->
            let dh = (int h + (360 - arg) / 90) % 4
            ((x, y), enum dh)
        | 'R' ->
            let dh = (int h + arg / 90) % 4
            ((x, y), enum dh)
        | _ -> invalidArg "action" "Invalid action"

    let solve (input: list<string>) =
        let (dx, dy) =
            input
            |> Seq.fold (fun (p, h) command -> maneuver p h command) ((0, 0), Heading.East)
            |> fst
        abs(dx) + abs(dy)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
