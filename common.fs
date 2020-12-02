module Common

    open System

    let readFromConsole = 
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun line -> (not (isNull line)))