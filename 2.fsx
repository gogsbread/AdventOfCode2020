#load "common.fs"

open System.Text.RegularExpressions

let re = Regex(@"(\d+)-(\d+) (\w): (\w+)")

type Policy =
    { Min: int
      Max: int
      Character: char
      Password: string }

module part1 =
    let parse (s: string) =
        let m = re.Match(s)

        let c =
            m.Groups
            |> Seq.skip 1
            |> Seq.map (fun g -> g.Captures.[0].Value)
            |> Seq.toList

        { Min = int c.[0]
          Max = int c.[1]
          Character = char c.[2]
          Password = c.[3] }

    let counter (s: string) =
        s.ToCharArray()
        |> Seq.fold (fun st c ->
            Map.change c (fun v ->
                match v with
                | Some e -> Some(e + 1)
                | None -> Some 1) st) Map.empty

    let inRange (p: Policy) =
        let counts = counter p.Password

        counts.ContainsKey p.Character
        && p.Min <= counts.[p.Character]
        && counts.[p.Character] <= p.Max

    let solve input =
        input
        |> Seq.map parse
        |> Seq.filter inRange
        |> Seq.length

let input = Common.readIn
input |> part1.solve |> Common.writeOut
