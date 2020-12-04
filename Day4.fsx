open System.Text.RegularExpressions
let input = System.IO.File.ReadAllText "inputs/day4.txt" |> fun s -> s.Split $"{System.Environment.NewLine}{System.Environment.NewLine}"

let correctFields = 
    input
    |> Array.filter (fun s ->
        s.Contains "byr" &&
        s.Contains "iyr" &&
        s.Contains "eyr" &&
        s.Contains "hgt" &&
        s.Contains "hcl" &&
        s.Contains "ecl" &&
        s.Contains "pid"
    )

correctFields
|> Array.length
|> printfn "Part 1: %A"

let byrPattern = @"byr:(\d{4})"
let iyrPattern = @"iyr:(\d{4})"
let eyrPattern = @"eyr:(\d{4})"
let hgtPattern = @"hgt:(\d{3}cm|\d{2}in)"
let hclPattern = @"hcl:#[0-9a-f]{6}"
let eclPattern = @"ecl:(amb|blu|brn|gry|grn|hzl|oth)"
let pidPattern = @"pid:(\d{9})(?!\d)" // Match only 9 numbers, no more (needs negative lookahead for the tenth number)

let yearValidation lower upper year =
    let year = int year
    lower <= year && year <= upper

let byrValidation = yearValidation 1920 2002
let iyrValidation  = yearValidation 2010 2020
let eyrValidation = yearValidation 2020 2030
let hgtValidation (heigth:string) =
    let height = int heigth.[..^2]
    match heigth.[^1..] with
    | "cm" -> 150 <= height && height <= 193
    | "in" -> 59 <= height && height <= 76
    | _ -> false

let tryValidate pattern validation input = 
    Regex.Matches(input, pattern)
    |> Seq.tryHead
    |> Option.exists(fun rgxMatch ->
        match validation with
        | None -> true // Don't validate, match is valid
        | Some validationRule -> validationRule rgxMatch.Groups.[1].Value
    )

correctFields
|> Array.filter (fun s ->
    tryValidate byrPattern (Some byrValidation) s &&
    tryValidate iyrPattern (Some iyrValidation) s &&
    tryValidate eyrPattern (Some eyrValidation) s &&
    tryValidate hgtPattern (Some hgtValidation) s &&
    tryValidate hclPattern None s &&
    tryValidate eclPattern None s &&
    tryValidate pidPattern None s )
|> Array.length
|> printfn "Part 2: %A" 