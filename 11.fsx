#load "common.fs"

type Seat =
    | Occupied
    | Empty
    | Floor

    static member empty (n: int) (m: int) =
        List.replicate n (List.replicate m Empty)

let parse (s: string) =
    s.ToCharArray()
    |> Seq.map (fun c ->
        match c with
        | '#' -> Occupied
        | '.' -> Floor
        | 'L' -> Empty
        | u -> (invalidArg (u.ToString()) "Unknow char"))
    |> Seq.toList

module part1 =
    let adjacents (r: int) (c: int) (seats: list<list<Seat>>) =
        let adj =
            (fun (i, j) ->
                let row = r + i
                let col = c + j
                if row < seats.Length
                   && row >= 0
                   && col < seats.[0].Length
                   && col >= 0
                   && (row, col) <> (r, c) then
                    Some seats.[row].[col]
                else
                    None)

        Seq.allPairs [ -1; 0; 1 ] [ -1; 0; 1 ]
        |> Seq.choose adj

    let transform (r: int) (c: int) (seats: list<list<Seat>>) =
        let seat = seats.[r].[c]
        match seat with
        | Empty ->
            let adjNotOccupied =
                Seq.forall (fun x -> x <> Occupied) (adjacents r c seats)

            if adjNotOccupied then Occupied else Empty
        | Occupied ->
            let fourMore =
                Seq.sumBy (fun x -> if x = Occupied then 1 else 0) (adjacents r c seats)
                >= 4

            if fourMore then Empty else Occupied
        | Floor -> Floor

    let solve (input: string list) =
        let seats = input |> List.map parse

        let gen (s: list<list<Seat>>) =
            let n = s.Length
            let m = s.[0].Length

            let newSeats =
                [ for i in [ 0 .. n - 1 ] do
                    [ for j in [ 0 .. m - 1 ] do
                        transform i j s ] ]

            if s <> newSeats then Some(newSeats, newSeats) else None

        Seq.unfold gen seats
        |> Seq.last
        |> Seq.collect id
        |> Seq.filter (fun x -> x = Occupied)
        |> Seq.length

let input = Common.readIn
input |> part1.solve |> Common.writeOut
