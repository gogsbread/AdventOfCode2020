#load "common.fs"

open System.Text.RegularExpressions

type Range = { Start: int; End: int }
type Field = { Name: string; Ranges: list<Range> }
type Ticket = list<int>

let parseField (s: string) =
    let rulesRe =
        Regex.Match(s, @"(.*)?: (\d+)-(\d+) or (\d+)-(\d+)")

    match rulesRe.Success with
    | true ->
        let name = rulesRe.Groups.[1].Value

        let range1 =
            { Start = int rulesRe.Groups.[2].Value
              End = int rulesRe.Groups.[3].Value }

        let range2 =
            { Start = int rulesRe.Groups.[4].Value
              End = int rulesRe.Groups.[5].Value }

        Some
            { Name = name
              Ranges = [ range1; range2 ] }
    | false -> None

let parseTicket (s: string): Ticket option =
    match s.Contains(",") with
    | true -> Some(s.Split(",") |> Array.map int |> Array.toList)
    | false -> None

module part1 =
    let solve (input: list<string>) =
        let fields = input |> List.choose parseField
        let tickets = input |> List.choose parseTicket
        let neighbours = tickets.Tail

        let outOfRange (ranges: list<Range>) (n: int): bool =
            ranges
            |> List.forall (fun r -> n < r.Start || r.End < n)

        let violatesRules (ticket: Ticket): option<int> =
            let ranges =
                fields |> List.collect (fun r -> r.Ranges)

            ticket
            |> List.tryPick (fun n -> if (outOfRange ranges n) then Some n else None)


        neighbours
        |> List.choose violatesRules
        |> List.sum

module part2 =
    let solve (input: list<string>) =
        let fields = input |> List.choose parseField
        let tickets = input |> List.choose parseTicket
        let yours :: neighbours = tickets

        let fieldsInRange (n: int) =
            Set.ofList fields
            |> Set.filter (fun f -> List.exists (fun r -> r.Start <= n && n <= r.End) f.Ranges)

        let possibleTicket (ticket: Ticket) =
            let possibilities = ticket |> List.map fieldsInRange
            if List.forall (fun (x: Set<Field>) -> x.Count > 0) possibilities
            then Some possibilities
            else None

        let rec fieldPositions (positionPossibilities: list<Set<Field>>)
                               (i: int)
                               (usedPossibilities: Set<Field>)
                               : option<list<Field>> =
            if i = positionPossibilities.Length then
                Some []
            else
                let unUsedPossibilities =
                    positionPossibilities.[i]
                    |> Set.filter (fun f -> not (Set.contains f usedPossibilities))
                    |> Set.toList

                unUsedPossibilities
                |> List.tryPick (fun f ->
                    let result =
                        fieldPositions positionPossibilities (i + 1) (usedPossibilities.Add(f))

                    match result with
                    | Some r -> Some(f :: r)
                    | _ -> None)

        let possibleTickets = neighbours |> List.choose possibleTicket

        let positionPossibilities =
            (seq {
                for i in [ 0 .. fields.Length - 1 ] do
                    Set.intersectMany
                        (seq {
                            for j in [ 0 .. possibleTickets.Length - 1 ] do
                                possibleTickets.[j].[i]
                         })
             }
             |> Seq.toList)

        let positions =
            (fieldPositions positionPossibilities 0 Set.empty).Value

        List.zip positions yours
        |> List.filter (fun (f, _) -> f.Name.StartsWith("departure"))
        |> List.fold (fun acc (_, v) -> acc * (int64 v)) 1L


let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
