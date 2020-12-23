// create 4 numbers from each tile
// type with l,r,t,b
// every tile will find either 2, 3 or 4 common numbers
// count the number of matches a tile gets
// filter tiles that only have 2 numbers

#load "common.fs"

open System.Text.RegularExpressions

module part1 =

    type Tile =
        | L of int
        | R of int
        | T of int
        | B of int

    type Camera =
        | L of option<int>
        | R of option<int>
        | T of option<int>
        | B of option<int>

    let tileRe = Regex(@"Tile (\d+):")

    let (|TileHeader|_|) (s: string) =
        let m = tileRe.Match(s)
        if m.Success then Some(int m.Groups.[1].Value) else None

    let (|TileValue|_|) (s: string) =
        if s.Trim().Length > 0 then Some s else None

    let parse (input: list<string>): Map<int, list<string>> =
        input
        |> List.fold (fun (cid, acc) s ->
            match s with
            | TileHeader id -> (id, Map.add id [] acc)
            | TileValue s ->
                let v = acc.[cid] @ [ s ]
                (cid, Map.add cid v acc)
            | _ -> (cid, acc)) (-1, Map.empty)
        |> snd

    let parseTileCorners (id: int) (tile: list<string>) =
        //
    let solve (input: list<string>) = parse input

let input = Common.readIn
input |> part1.solve |> Common.writeOut
