#load "common.fs"

open System
open System.Collections.Generic
open System.Text.RegularExpressions

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

        Some(n, ChoiceSubRules(sr1, sr2))
    elif subrulesRe.Success then
        let n = int subrulesRe.Groups.[1].Value

        let sr =
            subrulesRe.Groups.[2].Value.Split(' ')
            |> Array.map int
            |> Array.toList

        Some(n, SubRules sr)
    elif terminalRe.Success then
        let n = int terminalRe.Groups.[1].Value
        Some(n, Terminal terminalRe.Groups.[2].Value)
    else
        None

let parseMessage (s: string): option<string> =
    let messageRe = Regex.Match(s, @"^[a-b]+$")
    if messageRe.Success then Some messageRe.Value else None

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
    | ChoiceSubRules (sr1, sr2) ->
        (eval (SubRules sr1) rules)
        @ (eval (SubRules sr2) rules)

module part1 =
    let solve (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages = input |> List.choose parseMessage

        let validMessages = Set(eval rules.[0] rules)

        (messages |> List.filter validMessages.Contains)
            .Length


module part2 =
    let invalid (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages = input |> List.choose parseMessage

        let validMessages = Set(eval rules.[0] rules)

        messages
        |> List.filter (validMessages.Contains >> not)

    let solve (input: list<string>) =
        let rules =
            input
            |> List.choose parseRule
            |> List.fold (fun acc (n, r) -> Map.add n r acc) Map.empty

        let messages = input |> List.choose parseMessage

        let fortyTwoMessages = eval rules.[42] rules
        let thirtyOneMessages = eval rules.[31] rules
        let invalidMessages = invalid input

        let rec prefixMatch (s: string) (count: int) (l: list<Terminal>) =
            match List.tryFind (fun (ls: string) -> s.StartsWith(ls)) l with
            | Some m -> prefixMatch s.[m.Length..] (count + 1) l
            | None -> (count, s)

        let additional =
            (invalidMessages
             |> List.choose
                 (fun m ->
                     let (countFortyTwo, remaining) = prefixMatch m 0 fortyTwoMessages

                     if countFortyTwo < 2
                        || String.IsNullOrEmpty(remaining) then
                         None
                     else
                         let (countThirtyOne, rest) =
                             prefixMatch remaining 0 thirtyOneMessages

                         if countThirtyOne >= 1
                            && countFortyTwo > countThirtyOne
                            && String.IsNullOrEmpty(rest) then
                             Some m
                         else
                             None))
                .Length

        part1.solve input + additional

let input = Common.readIn
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut
