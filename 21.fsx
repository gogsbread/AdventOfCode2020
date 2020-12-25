#load "common.fs"

open System.Linq

let parse (s: string) =
    let ing = s.Split(" (contains ").[0].Split(' ')

    let alg =
        s.Split(" (contains ").[1].TrimEnd(')').Split(", ")

    ing, alg

module part1 =
    let solve (input: list<string>) =
        let ing =
            input
            |> List.map parse
            |> List.fold (fun acc (ing, _) -> Set.union (Set.ofArray ing) acc) Set.empty

        let alg =
            input
            |> List.map parse
            |> List.fold (fun acc (_, alg) -> Set.union (Set.ofArray alg) acc) Set.empty

        let ingWitAlg =
            alg
            |> Seq.fold (fun acc a ->
                let v =
                    input
                    |> List.map parse
                    |> List.filter (fun (_, alg) -> alg.Contains(a))
                    |> List.map (fst >> Set.ofArray)
                    |> List.reduce Set.intersect

                Set.union v acc) Set.empty

        let ingWithoutAlg = Set.difference ing ingWitAlg

        ingWithoutAlg
        |> Seq.sumBy (fun a ->
            (input
             |> List.map parse
             |> List.filter (fun (ing, _) -> ing.Contains(a))).Length)

module part2 =
    let solve (input: list<string>) =
        //  Use the partial solution below to solve the rest by hand
        // You can also sort by the lenght of set in the map and then go about
        // reducing the values from other ones
        let ing =
            input
            |> List.map parse
            |> List.fold (fun acc (ing, _) -> Set.union (Set.ofArray ing) acc) Set.empty

        let alg =
            input
            |> List.map parse
            |> List.fold (fun acc (_, alg) -> Set.union (Set.ofArray alg) acc) Set.empty

        alg
        |> Seq.fold (fun acc a ->
            let v =
                input
                |> List.map parse
                |> List.filter (fun (_, alg) -> alg.Contains(a))
                |> List.map (fst >> Set.ofArray)
                |> List.reduce Set.intersect

            Map.add a v acc) Map.empty

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
