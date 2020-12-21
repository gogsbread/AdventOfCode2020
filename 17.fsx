// find all the adjacents with x, y, z
// find out how many are activie 

type Cube =
    | Active
    | Inactive

type Point3D = {
    X: int
    Y: int
    Z: int
}

module part1 =
    let parse (s: string): list<Cube> =
        s
        |> Seq.map(fun c ->
            match c with
            | '.' -> Inactive
            |'#' -> Active
            |_ -> invalidArg "c" "Unknown input")
        |> Seq.toList

    let adjacents (x: int) (y: int) (z: int) 
    let solve (input: list<string>) =
        input
        |> List.map parse
