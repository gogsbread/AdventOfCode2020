#load "common.fs"

type Point = { X: float; Y: float }

let rec tileID (cell: Point) (directions: list<char>) =
    match directions with
    | [] -> cell
    | 'e' :: rest -> tileID { X = cell.X + 1.0; Y = cell.Y } rest
    | 's' :: 'e' :: rest -> tileID { X = cell.X + 0.5; Y = cell.Y - 1.0 } rest
    | 's' :: 'w' :: rest -> tileID { X = cell.X - 0.5; Y = cell.Y - 1.0 } rest
    | 'w' :: rest -> tileID { X = cell.X - 1.0; Y = cell.Y } rest
    | 'n' :: 'w' :: rest -> tileID { X = cell.X - 0.5; Y = cell.Y + 1.0 } rest
    | 'n' :: 'e' :: rest -> tileID { X = cell.X + 0.5; Y = cell.Y + 1.0 } rest
    | _ -> invalidArg "directions" "Invalid direction"

module part1 =
    let solve (input: list<string>) =
        input
        |> List.map (fun x -> tileID { X = 0.0; Y = 0.0 } (x |> Seq.toList))
        |> List.fold (fun (floor: Map<Point, bool>) (tile: Point) ->
            if floor.ContainsKey(tile) then floor.Add(tile, (not floor.[tile])) else floor.Add(tile, true)) Map.empty
        |> Map.filter (fun _ v -> v)
        |> Map.count

module part2 =
    let directions =
        [ [ 'e' ]
          [ 's'; 'e' ]
          [ 's'; 'w' ]
          [ 'w' ]
          [ 'n'; 'w' ]
          [ 'n'; 'e' ] ]

    let adjacents (tile: Point) =
        seq {
            for d in directions do
                tileID tile d
        }

    let solve (input: list<string>) =
        let day0 =
            input
            |> List.map (fun x -> tileID { X = 0.0; Y = 0.0 } (x |> Seq.toList))
            |> List.fold (fun (floor: Map<Point, bool>) (tile: Point) ->
                if floor.ContainsKey(tile) then floor.Add(tile, (not floor.[tile])) else floor.Add(tile, true))
                   Map.empty

        seq { 1 .. 100 }
        |> Seq.fold (fun map _ ->
            map
            |> Map.toSeq
            |> Seq.collect (fun (p: Point, black: bool) ->
                Seq.append
                    (seq { yield (p, black) })
                    (adjacents p
                     |> Seq.map (fun t -> (t, (if map.ContainsKey(t) then map.[t] else false)))))
            |> Map.ofSeq
            |> Map.fold (fun (floor: Map<Point, bool>) (p: Point) (black: bool) ->
                let adjBlacks =
                    adjacents p
                    |> Seq.sumBy (fun t -> if map.ContainsKey(t) then (if map.[t] then 1 else 0) else 0)

                if black && (adjBlacks = 0 || adjBlacks > 2)
                then floor.Add(p, false)
                elif (not black) && adjBlacks = 2
                then floor.Add(p, true)
                else floor.Add(p, black)) Map.empty) day0
        |> Map.filter (fun _ v -> v)
        |> Map.count

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
