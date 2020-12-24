#load "common.fs"

open System.Text.RegularExpressions

type TileID = int

let tileRe = Regex(@"Tile (\d+):")

let (|TileHeader|_|) (s: string) =
    let m = tileRe.Match(s)
    if m.Success then Some(int m.Groups.[1].Value) else None

let (|TileValue|_|) (s: string) =
    if s.Trim().Length > 0 then Some s else None

let parse (input: list<string>): Map<TileID, list<string>> =
    input
    |> List.fold (fun (cid, acc) s ->
        match s with
        | TileHeader id -> (id, Map.add id [] acc)
        | TileValue s ->
            let v = acc.[cid] @ [ s ]
            (cid, Map.add cid v acc)
        | _ -> (cid, acc)) (-1, Map.empty)
    |> snd

type TileBorder = { L: int; R: int; T: int; B: int }

type Tile = { ID: TileID; Border: TileBorder }

type Camera =
    { ID: TileID
      L: option<TileID>
      R: option<TileID>
      T: option<TileID>
      B: option<TileID> }

let parseTile (id: TileID) (tile: list<string>): list<Tile> =
    let folder (acc: int) (c: char) =
        match c with
        | '.' -> acc <<< 1
        | '#' -> (acc <<< 1) ||| 1
        | _ -> invalidArg "c" "Invalid input"

    let top = tile.[0] |> Seq.fold folder 0
    let rtop = tile.[0] |> Seq.rev |> Seq.fold folder 0

    let left =
        Seq.map (fun (s: string) -> s.[0]) tile
        |> Seq.fold folder 0

    let rleft =
        Seq.map (fun (s: string) -> s.[0]) tile
        |> Seq.rev
        |> Seq.fold folder 0

    let right =
        Seq.map (fun (s: string) -> s.[s.Length - 1]) tile
        |> Seq.fold folder 0

    let rright =
        Seq.map (fun (s: string) -> s.[s.Length - 1]) tile
        |> Seq.rev
        |> Seq.fold folder 0

    let bottom = Seq.last tile |> Seq.fold folder 0

    let rbottom =
        Seq.last tile |> Seq.rev |> Seq.fold folder 0

    [ { ID = id
        Border =
            { L = right
              R = left
              T = rtop
              B = rbottom } }
      { ID = id
        Border =
            { L = rleft
              R = rright
              T = bottom
              B = top } } ]

let isCameraBorder (camera: Camera) =
    let l = if camera.L.IsNone then 1 else 0
    let r = if camera.R.IsNone then 1 else 0
    let t = if camera.T.IsNone then 1 else 0
    let b = if camera.B.IsNone then 1 else 0
    l + r + t + b = 2

let camera (tile: Tile) (tiles: list<Tile>)=
        let border = tile.Border

        let otherTiles = tiles |> List.except [ tile ]

        let left =
            List.tryFind (fun t ->
                border.L = t.Border.L
                || border.L = t.Border.R
                || border.L = t.Border.T
                || border.L = t.Border.B) otherTiles

        let right =
            List.tryFind (fun t ->
                border.R = t.Border.L
                || border.R = t.Border.R
                || border.R = t.Border.T
                || border.R = t.Border.B) otherTiles

        let top =
            List.tryFind (fun t ->
                border.T = t.Border.L
                || border.T = t.Border.R
                || border.T = t.Border.T
                || border.T = t.Border.B) otherTiles

        let bottom =
            List.tryFind (fun t ->
                border.B = t.Border.L
                || border.B = t.Border.R
                || border.B = t.Border.T
                || border.B = t.Border.B) otherTiles

        { ID = tile.ID
          L = if left.IsSome then Some left.Value.ID else None
          R = if right.IsSome then Some right.Value.ID else None
          T = if top.IsSome then Some top.Value.ID else None
          B = if bottom.IsSome then Some bottom.Value.ID else None }

let rec cameraArray (connectingCameras: list<Camera>) (cameras: list<Camera>) =
    match connectingCameras with
    | [] -> []
    | head::rest -> 
        printfn "%A" head 
        printfn "%A" cameras
        let l = if head.L.IsSome then [(List.find (fun c -> c.ID = head.L.Value && c.R.IsSome && c.R.Value = head.ID) cameras)] else []
        let r = if head.R.IsSome then [(List.find (fun c -> c.ID = head.R.Value && c.L.IsSome && c.L.Value = head.ID) cameras)] else []
        let t = if head.T.IsSome then [(List.find (fun c -> c.ID = head.T.Value && c.B.IsSome && c.B.Value = head.ID) cameras)] else []
        let b = if head.B.IsSome then [(List.find (fun c -> c.ID = head.B.Value && c.T.IsSome && c.T.Value = head.ID) cameras)] else []
        let more = rest @ l @ r @ b @ t
        head::cameraArray more cameras

module part1 =
    // Since part1 asks for corners, this partial construction will work.
    // for part2, I have to rotate in all directions and flip rotate in all directions
    let solve (input: Map<TileID, list<string>>) =
        let tiles =
            input
            |> Map.toList
            |> List.collect (fun (id, tile) -> parseTile id tile)

        tiles
        |> List.map (fun t -> camera t tiles)
        |> List.filter isCameraBorder
        |> List.distinctBy (fun t -> t.ID)
        |> List.map (fun t -> t.ID)
        |> List.fold (fun acc id -> acc * int64 id) 1L

let input = Common.readIn
input |> parse |> part1.solve |> Common.writeOut
