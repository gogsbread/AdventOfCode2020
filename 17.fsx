// find all the adjacents with x, y, z
// find out how many are activie

#load "common.fs"


type State =
    | Active
    | Inactive

let parse (s: string): seq<State> =
    s
    |> Seq.map (fun c ->
        match c with
        | '.' -> Inactive
        | '#' -> Active
        | _ -> invalidArg "c" "Unknown input")

module part1 =
    type Point3D = { X: int; Y: int; Z: int }
    type Cube = { Point: Point3D; State: State }

    let solve (input: list<string>) =
        let adjacents (c: Cube) (cubes: Map<Point3D, Cube>): seq<Cube> =
            seq {
                for i in seq { -1 .. 1 } do
                    for j in seq { -1 .. 1 } do
                        for k in seq { -1 .. 1 } do
                            if not (i = 0 && j = 0 && k = 0) then
                                let p =
                                    { X = c.Point.X + i
                                      Y = c.Point.Y + j
                                      Z = c.Point.Z + k }

                                if cubes.ContainsKey p then cubes.[p] else { Point = p; State = Inactive }
            }

        let activeAdjacents (c: Cube) (cubes: Map<Point3D, Cube>): int =
            adjacents c cubes
            |> Seq.sumBy (fun ac ->
                if cubes.ContainsKey ac.Point
                then if cubes.[ac.Point].State = Active then 1 else 0
                else 0)

        let next (c: Cube) (cubes: Map<Point3D, Cube>): Cube =
            let n = activeAdjacents c cubes
            match c.State with
            | Active -> if n = 2 || n = 3 then { c with State = Active } else { c with State = Inactive }
            | Inactive -> if n = 3 then { c with State = Active } else { c with State = Inactive }

        let cubes =
            input
            |> List.map parse
            |> Seq.fold (fun (i, acc) row ->
                let indexedCols = (row |> Seq.mapi (fun k x -> (k, x)))
                (i + 1,
                 acc
                 @ [ for (j, st) in indexedCols do
                         { Point = { X = i; Y = j; Z = 0 }
                           State = st } ])) (0, List.empty)
            |> snd
            |> Seq.fold (fun acc c -> Map.add c.Point c acc) Map.empty

        seq { 1 .. 6 }
        |> Seq.fold (fun cs _ ->
            let adjCubes =
                cs
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.fold (fun st c ->
                    adjacents c st
                    |> Seq.fold (fun acc c -> Map.add c.Point c acc) st) cs

            Map.map (fun p c -> next c adjCubes) adjCubes) cubes
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sumBy (fun c -> if c.State = Active then 1 else 0)

// Just copy pasted the solution above to make a Point4D. Didn't have patience to impl a Nd array
module part2 =
    type Point4D = { X: int; Y: int; Z: int; W: int }
    type Cube = { Point: Point4D; State: State }

    let solve (input: list<string>) =
        let adjacents (c: Cube) (cubes: Map<Point4D, Cube>): seq<Cube> =
            seq {
                for i in seq { -1 .. 1 } do
                    for j in seq { -1 .. 1 } do
                        for k in seq { -1 .. 1 } do
                            for w in seq { -1 .. 1 } do
                                if not (i = 0 && j = 0 && k = 0 && w = 0) then
                                    let p =
                                        { X = c.Point.X + i
                                          Y = c.Point.Y + j
                                          Z = c.Point.Z + k
                                          W = c.Point.W + w }

                                    if cubes.ContainsKey p then cubes.[p] else { Point = p; State = Inactive }
            }

        let activeAdjacents (c: Cube) (cubes: Map<Point4D, Cube>): int =
            adjacents c cubes
            |> Seq.sumBy (fun ac ->
                if cubes.ContainsKey ac.Point
                then if cubes.[ac.Point].State = Active then 1 else 0
                else 0)

        let next (c: Cube) (cubes: Map<Point4D, Cube>): Cube =
            let n = activeAdjacents c cubes
            match c.State with
            | Active -> if n = 2 || n = 3 then { c with State = Active } else { c with State = Inactive }
            | Inactive -> if n = 3 then { c with State = Active } else { c with State = Inactive }

        let cubes =
            input
            |> List.map parse
            |> Seq.fold (fun (i, acc) row ->
                let indexedCols = (row |> Seq.mapi (fun k x -> (k, x)))
                (i + 1,
                 acc
                 @ [ for (j, st) in indexedCols do
                         { Point = { X = i; Y = j; Z = 0; W = 0 }
                           State = st } ])) (0, List.empty)
            |> snd
            |> Seq.fold (fun acc c -> Map.add c.Point c acc) Map.empty

        seq { 1 .. 6 }
        |> Seq.fold (fun cs _ ->
            let adjCubes =
                cs
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.fold (fun st c ->
                    adjacents c st
                    |> Seq.fold (fun acc c -> Map.add c.Point c acc) st) cs

            Map.map (fun p c -> next c adjCubes) adjCubes) cubes
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sumBy (fun c -> if c.State = Active then 1 else 0)

let input = Common.readIn
// input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
