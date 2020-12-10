#load "common.fs"

module part1 =
    let solve (input: List<string>) =
        let expenses = input |> List.map int
        List.allPairs expenses expenses
        |> List.pick (fun (x, y) -> if x + y = 2020 then Some(x * y) else None)

module part2 =
    let solve input =
        let expenses = input |> List.map int
        let pairs = List.allPairs expenses expenses
        List.pick (fun (x, y, z) -> if x + y + z = 2020 then Some(x * y * z) else None)
            [ for x in expenses do
                for (y, z) in pairs do
                    (x, y, z) ]

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
