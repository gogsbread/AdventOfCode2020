#load "common.fs"

open System.Text.RegularExpressions
open System.Collections.Generic

type Terminal = string
type SubRule = int
type SubRules = list<SubRule>

type Rule =
    | Terminal of Terminal
    | SubRules of SubRules
    | ChoiceSubRules of SubRules * SubRules

let parseRule (s: string): option<(SubRule * Rule)> =
    let subrulesRe = Regex.Match(s, @"(\d+): (\d+(\s\d+)*)")
    let choiceSubrulesRe =
        Regex.Match(s, @"(\d+): (\d+(\s\d+)*)\s\|\s(\d+(\s\d+)*)")
    let terminalRe = Regex.Match(s, @"(\d+): ""(\w)""")

    if choiceSubrulesRe.Success then
        let n = int choiceSubrulesRe.Groups.[1].Value

        let sr1 =
            choiceSubrulesRe.Groups.[2].Value.Split(' ')
            |> Array.map int
            |> Array.toList

        let sr2 =
            choiceSubrulesRe.Groups.[4].Value.Split(' ')
            |> Array.map int
            |> Array.toList

        Some (n, ChoiceSubRules(sr1, sr2))
    elif subrulesRe.Success then
        let n = int subrulesRe.Groups.[1].Value

        let sr =
            subrulesRe.Groups.[2].Value.Split(' ')
            |> Array.map int
            |> Array.toList

        Some (n, SubRules sr)
    elif terminalRe.Success then
        let n = int terminalRe.Groups.[1].Value
        Some (n, Terminal terminalRe.Groups.[2].Value)
    else
        None

let parseMessage(s: string):  option<string> =
    let messageRe = Regex.Match(s,  @"^[a-b]+$")
    if messageRe.Success then
        Some messageRe.Value
    else
        None

let cache = Dictionary<SubRule, list<Terminal>>()

let rec eval (r: Rule) (rules: Map<SubRule, Rule>): list<Terminal> =
    match r with
    | Terminal t -> [ t ]
    | SubRules [] -> []
    | SubRules [ sr ] ->
        if cache.ContainsKey(sr) then
            cache.[sr]
        else
            let value = eval rules.[sr] rules
            cache.Add(sr, value)
            value
    | SubRules (h :: rest) ->
        List.allPairs (eval (SubRules [ h ]) rules) (eval (SubRules rest) rules)
        |> List.map (fun (a, b) -> a + b)
    | ChoiceSubRules (sr1, sr2) -> (eval (SubRules sr1) rules) @ (eval (SubRules sr2) rules)

module part1 =
    let solve (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages =
            input
            |> List.choose parseMessage

        let validMessages = Set (eval rules.[0] rules)

        (messages
        |> List.filter validMessages.Contains).Length

module part2 =
    let invalid (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages =
            input
            |> List.choose parseMessage

        let validMessages = Set (eval rules.[0] rules)

        messages
        |> List.filter (validMessages.Contains >> not)

    let eight (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages =
            input
            |> List.choose parseMessage

        let fortyTwoMessages = eval rules.[42] rules
        let thirtyOneMessages = eval rules.[31] rules
        fortyTwoMessages @ thirtyOneMessages
        // for each message 
        //  length / 8 (len 42 or 31)
        //  enumerate over n_42 = 3 and n_31 = 1
        // let invalidMessages = invalid input

        // invalidMessages
        // |> List.choose
        //     (fun m ->
        //         match List.tryFind (fun (ftM: string) -> m.StartsWith(ftM)) fortyTwoMessages with
        //         | Some _ -> 
        //             match List.tryFind (fun (toM: string) -> m.EndsWith(toM)) thirtyOneMessages with
        //             | Some _ -> Some m
        //             | None -> None
        //         | None -> None
        //     )

let input = Common.readIn
// input  |> List.choose part1.parseRule |> List.iter (printfn "%A")
// input |> part1.solve |> Common.writeOut
input |> part2.eight |> List.iter (printfn "%A")
