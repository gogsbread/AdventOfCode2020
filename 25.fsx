#load "common.fs"

module part1 =
    let solve (input: list<string>) =
        let cardPK = int64 input.[0]
        let doorPK = int64 input.[1]

        let rec loopSize (subject: int64) (target: int64) (cv: int64) (n: int)=
            if cv = target then
                n
            else
                loopSize subject target ((cv * subject) % 20201227L) (n+1)

        let transform (subject: int64) (loop: int) =
            seq { 1 .. loop }
            |> Seq.fold (fun (cv: int64) _ ->
                (cv * subject) % 20201227L) 1L

        let cardLoop = loopSize 7L cardPK 1L 0
        let doorLoop = loopSize 7L doorPK 1L 0

        transform doorPK cardLoop, transform cardPK doorLoop

let input = Common.readIn
input |> part1.solve |> Common.writeOut