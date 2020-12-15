#load "common.fs"

type Heading =
    | North = 0
    | South = 2
    | East = 1
    | West = 3

let direction (h: Heading) =
    match h with
    | Heading.North -> (0, 1)
    | Heading.South -> (0, -1)
    | Heading.East -> (1, 0)
    | Heading.West -> (-1, 0)
    | _ -> invalidArg "h" "Nan"

module part1 =
    let maneuver (x, y) (h: Heading) (command: string) =
        let action = command.[0]
        let arg = int (command.Substring 1)
        match action with
        | 'N' ->
            let dx, dy = direction Heading.North
            ((arg * dx + x, arg * dy + y), h)
        | 'S' ->
            let dx, dy = direction Heading.South
            ((arg * dx + x, arg * dy + y), h)
        | 'E' ->
            let dx, dy = direction Heading.East
            ((arg * dx + x, arg * dy + y), h)
        | 'W' ->
            let dx, dy = direction Heading.West
            ((arg * dx + x, arg * dy + y), h)
        | 'F' ->
            let dx, dy = direction h
            ((arg * dx + x, arg * dy + y), h)
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

        abs (dx) + abs (dy)

module part2 =
    type Point = { X: int; Y: int }
    type Nav = { Waypoint: Point; Ship: Point }

    let rec rotateCW (p: Point) (times: int) =
        if times = 0
        then p
        else rotateCW { X = p.Y; Y = (-1 * p.X) } (times - 1)

    let maneuver (n: Nav) (command: string) =
        let action = command.[0]
        let arg = int (command.Substring 1)
        match action with
        | 'N' ->
            let dx, dy = direction Heading.North
            { Waypoint =
                  { X = arg * dx + n.Waypoint.X
                    Y = arg * dy + n.Waypoint.Y }
              Ship = n.Ship }
        | 'S' ->
            let dx, dy = direction Heading.South
            { Waypoint =
                  { X = arg * dx + n.Waypoint.X
                    Y = arg * dy + n.Waypoint.Y }
              Ship = n.Ship }
        | 'E' ->
            let dx, dy = direction Heading.East
            { Waypoint =
                  { X = arg * dx + n.Waypoint.X
                    Y = arg * dy + n.Waypoint.Y }
              Ship = n.Ship }
        | 'W' ->
            let dx, dy = direction Heading.West
            { Waypoint =
                  { X = arg * dx + n.Waypoint.X
                    Y = arg * dy + n.Waypoint.Y }
              Ship = n.Ship }
        | 'F' ->
            { Waypoint = n.Waypoint
              Ship =
                  { X = arg * n.Waypoint.X + n.Ship.X
                    Y = arg * n.Waypoint.Y + n.Ship.Y } }
        | 'L' ->
            let times = ((360 - arg) / 90) % 4
            let p = rotateCW n.Waypoint times
            { Waypoint = p; Ship = n.Ship }
        | 'R' ->
            let times = (arg / 90) % 4
            let p = rotateCW n.Waypoint times
            { Waypoint = p; Ship = n.Ship }
        | _ -> invalidArg "action" "Invalid action"

    let solve (input: list<string>) =
        let p =
            input
            |> Seq.fold
                (maneuver)
                   { Waypoint = { X = 10; Y = 1 }
                     Ship = { X = 0; Y = 0 } }

        abs (p.Ship.X) + abs (p.Ship.Y)

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
