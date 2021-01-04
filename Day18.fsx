#nowarn "0025"
open System.Text.RegularExpressions
#time
let input = System.IO.File.ReadAllLines "inputs/day18.txt"

type Expr =
    | Digit of int64
    | Add of Expr * Expr
    | Mult of Expr * Expr

let rec eval = function
    | Digit value -> value
    | Add(a,b) -> (eval a) + (eval b)
    | Mult(a,b) -> (eval a) * (eval b)

let preProcess : string -> string =
    Seq.rev >> Seq.choose (function | ' ' -> None | ')' -> Some '(' | '(' -> Some ')' | x -> Some x) >> System.String.Concat

let preprocess2 : string -> string = 
    Seq.rev
    >> Seq.choose (function | ' ' -> None | '(' -> Some "))" | ')' -> Some "((" | '*' -> Some ")*(" | x -> Some (string x))
    >> System.String.Concat
    >> sprintf "(%s)"

let pattern = @"\(((?<BR>\()|(?<-BR>\))|[^()]*)+\)"
let rgx = Regex(pattern, RegexOptions.Compiled ||| RegexOptions.Singleline)

let findMatching inp =
    rgx.Match(inp).Value

let splitTerm inp =
    match Seq.toList inp with
    | '('::_ ->
        let f = (findMatching inp).[1..^1]
        f, System.String.Concat inp.[f.Length+2..]
    | _ ->
        string inp.[0], inp.Substring(1)

let rec parse (inp : string) =
    match inp.Contains('*') || inp.Contains('+') with
    | false -> 
        Digit (int64 (inp.Replace("(","").Replace(")","")))
    | true ->
        match splitTerm inp with
        | first, "" -> parse first
        | first, second -> 
            let rest = System.String.Concat second.[1..]
            match second.[0] with
            | '+'-> Add (parse first, parse rest)
            | '*' -> Mult (parse first, parse rest)

let solveWithPreprocess f = f >> parse >> eval

input
|> Array.sumBy (solveWithPreprocess preProcess)
|> printfn "Part 1: %i"

input
|> Array.sumBy (solveWithPreprocess preprocess2)
|> printfn "Part 2: %i"