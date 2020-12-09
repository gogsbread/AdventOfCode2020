#load "common.fs"

open System.Text.RegularExpressions

type Content = { Name: string; Count: int }

let parse (input: string) =
    let container =
        Regex.Match(input, @"(\w+ \w+) bags contain").Groups.[1].Value

    let contents =
        Regex.Matches(input, @"(\d+) (\w+ \w+) bag(s)?")
        |> Seq.map (fun m ->
            { Name = m.Groups.[2].Value
              Count = int (m.Groups.[1].Value) })
        |> Seq.toList

    (container, contents)

module part1 =
    let rec containersOf (bag: string) (containerMappings: Map<string, List<string>>) (knownContainers: Set<string>) =
        match containerMappings.ContainsKey(bag) with
        | true ->
            containerMappings.[bag]
            |> List.fold (fun (acc: Set<string>) (b: string) ->
                    containersOf b containerMappings (acc.Add b)) knownContainers
        | false -> knownContainers

    let solve (input: List<string>) =
        let rec vToU u v initialState =
            match v with
            | [] -> initialState
            | v1 :: rest ->
                let m =
                    Map.change v1.Name (fun t ->
                        match t with
                        | Some x -> Some(x @ [ u ])
                        | None -> Some [ u ]) initialState

                vToU u rest m

        let containerMappings =
            input
            |> List.map parse
            |> List.fold (fun acc (u, v) -> vToU u v acc) Map.empty

        containersOf "shiny gold" containerMappings Set.empty
        |> Set.count

module part2 =
    let solve (input: List<string>) =
        let rec contains (bag: string) (containerMappings: Map<string, List<Content>>) =
            List.sumBy (fun c -> c.Count + (c.Count * (contains c.Name containerMappings))) containerMappings.[bag]

        let mappings =
            input
            |> List.map parse
            |> Map.ofList

        contains "shiny gold" mappings

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
