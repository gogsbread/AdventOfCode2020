#load "common.fs"

module part1 =
    let solve input =
        input 
        |> Seq.iter (fun x -> printfn "%A" x)

Common.readFromConsole
|> part1.solve