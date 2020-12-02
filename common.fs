module Common

    open System

    let readIn = 
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun line -> (not (isNull line)))
        |> Seq.toList

    let writeOut output =
        printfn "%A" output