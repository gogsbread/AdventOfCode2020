#load "common.fs"

open System
open System.Text.RegularExpressions

type Passport =
    { Byr: int option
      Iyr: int option
      Eyr: int option
      Hgt: string option
      Hcl: string option
      Ecl: string option
      Pid: string option
      Cid: string option}
    static member Default =
        {
            Byr = None
            Iyr = None
            Eyr = None
            Hgt = None
            Hcl = None
            Ecl = None
            Pid = None
            Cid = None
        }
 
let (|PassportField|NotPassportField|) (str: string) =
    if not (String.IsNullOrWhiteSpace (str)) then
        PassportField str
    else
        NotPassportField

module part1 =
    // Have to do many active patterns instead of a single union active pattern because of <7 limitation
    let (|Byr|_|) (str: string) =
        let m = Regex.Match(str, @"byr:(\d+)")
        if m.Success then Some (int m.Groups.[1].Captures.[0].Value) else None
    let (|Iyr|_|) (str: string) =
        let m = Regex.Match(str, @"iyr:(\d+)")
        if m.Success then Some (int m.Groups.[1].Captures.[0].Value) else None
    let (|Eyr|_|) (str: string) =
        let m = Regex.Match(str, @"eyr:(\d+)")
        if m.Success then Some (int m.Groups.[1].Captures.[0].Value) else None
    let (|Hgt|_|) (str: string) =
        let m = Regex.Match(str, @"hgt:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None
    let (|Hcl|_|) (str: string) =
        let m = Regex.Match(str, @"hcl:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None
    let (|Ecl|_|) (str: string) =
        let m = Regex.Match(str, @"ecl:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None
    let (|Pid|_|) (str: string) =
        let m = Regex.Match(str, @"pid:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None
    let (|Cid|_|) (str: string) =
        let m = Regex.Match(str, @"cid:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None

    let toPassport (input: string list) =
        let s =
            input
            |> List.fold (fun st x -> st + " " + x) ""
        s.Split(" ")
        |> Array.fold (fun st x ->
            match x with
            | Byr b -> {st with Byr = Some b}
            | Iyr i -> {st with Iyr = Some i}
            | Eyr e -> {st with Eyr = Some e}
            | Hgt h -> {st with Hgt = Some h}
            | Hcl h -> {st with Hcl = Some h}
            | Ecl e -> {st with Ecl = Some e}
            | Pid p -> {st with Pid = Some p}
            | Cid c -> {st with Cid = Some c}
            | _ -> st ) Passport.Default

    let rec parse input =
        match input with
        | [] -> []
        | (PassportField p)::NotPassportField::rest -> toPassport [p]::(parse rest)
        | (PassportField p1)::(PassportField p2)::NotPassportField::rest -> toPassport [p1;p2]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::NotPassportField::rest -> toPassport [p1;p2;p3]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::NotPassportField::rest -> toPassport [p1;p2;p3;p4]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::(PassportField p7)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6;p7]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::(PassportField p7)::(PassportField p8)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6;p7;p8]::(parse rest)
        | _ -> [toPassport input]

    let solve input =
        input
        |> parse
        |> Seq.fold (fun st x ->
            match x with
            | {Byr = (Some _); Iyr = (Some _); Eyr = (Some _); Hgt = (Some _); Hcl = (Some _); Ecl = (Some _); Pid = (Some _)} -> 1 + st
            | _ -> st
        ) 0

// Duplicated a bunch of stuff; didn't want to refactor this guy
module part2 =
    // Have to do many active patterns instead of a single union active pattern because of <7 limitation
    let (|Byr|_|) (str: string) =
        let m = Regex.Match(str, @"byr:(\d{4}\b)")
        if m.Success
        then 
            let v = (int m.Groups.[1].Captures.[0].Value) 
            if 1920 <= v && v <= 2002 then
                Some v
            else
                None
        else
            None

    let (|Iyr|_|) (str: string) =
        let m = Regex.Match(str, @"iyr:(\d{4}\b)")
        if m.Success
        then 
            let v = (int m.Groups.[1].Captures.[0].Value) 
            if 2010 <= v && v <= 2020 then
                Some v
            else
                None
        else
            None

    let (|Eyr|_|) (str: string) =
        let m = Regex.Match(str, @"eyr:(\d{4}\b)")
        if m.Success
        then 
            let v = (int m.Groups.[1].Captures.[0].Value) 
            if 2020 <= v && v <= 2030 then
                Some v
            else
                None
        else
            None

    let (|Hgt|_|) (str: string) =
        let m = Regex.Match(str, @"hgt:(\d+)(cm|in)")
        if m.Success
        then 
            let v = (int m.Groups.[1].Captures.[0].Value)
            let u = m.Groups.[2].Captures.[0].Value
            if u = "cm" && 150 <= v && v <= 193 then
                Some (string v)
            elif u = "in" && 59 <= v && v <= 76 then
                Some (string v)
            else
                None
        else
            None

    let (|Hcl|_|) (str: string) =
        let m = Regex.Match(str, @"hcl:#([a-f0-9]{6}\b)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None

    let (|Ecl|_|) (str: string) =
        let m = Regex.Match(str, @"ecl:(amb|blu|brn|gry|grn|hzl|oth)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None

    let (|Pid|_|) (str: string) =
        let m = Regex.Match(str, @"pid:(\d{9}\b)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None

    let (|Cid|_|) (str: string) =
        let m = Regex.Match(str, @"cid:(.+)")
        if m.Success then Some (m.Groups.[1].Captures.[0].Value) else None

    let toPassport (input: string list) =
        let s =
            input
            |> List.fold (fun st x -> st + " " + x) ""
        s.Split(" ")
        |> Array.fold (fun st x ->
            match x with
            | Byr b -> {st with Byr = Some b}
            | Iyr i -> {st with Iyr = Some i}
            | Eyr e -> {st with Eyr = Some e}
            | Hgt h -> {st with Hgt = Some h}
            | Hcl h -> {st with Hcl = Some h}
            | Ecl e -> {st with Ecl = Some e}
            | Pid p -> {st with Pid = Some p}
            | Cid c -> {st with Cid = Some c}
            | _ -> st ) Passport.Default

    let rec parse input =
        match input with
        | [] -> []
        | (PassportField p)::NotPassportField::rest -> toPassport [p]::(parse rest)
        | (PassportField p1)::(PassportField p2)::NotPassportField::rest -> toPassport [p1;p2]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::NotPassportField::rest -> toPassport [p1;p2;p3]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::NotPassportField::rest -> toPassport [p1;p2;p3;p4]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::(PassportField p7)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6;p7]::(parse rest)
        | (PassportField p1)::(PassportField p2)::(PassportField p3)::(PassportField p4)::(PassportField p5)::(PassportField p6)::(PassportField p7)::(PassportField p8)::NotPassportField::rest -> toPassport [p1;p2;p3;p4;p5;p6;p7;p8]::(parse rest)
        | _ -> [toPassport input]

    let solve input =
        input
        |> parse
        |> Seq.fold (fun st x ->
            match x with
            | {Byr = (Some _); Iyr = (Some _); Eyr = (Some _); Hgt = (Some _); Hcl = (Some _); Ecl = (Some _); Pid = (Some _)} -> 1 + st
            | _ -> st
        ) 0

let input = Common.readIn
// printfn "%A" (input |> part1.parse)
input |> part1.solve |> Common.writeOut
input |> part2.solve |> Common.writeOut