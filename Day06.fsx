open System
let nl = Environment.NewLine
let input = IO.File.ReadAllText "inputs/day06.txt" |> fun s -> s.Split $"{nl}{nl}"

#time
let countAnyMatches : string -> int =
    Seq.filter Char.IsLetter
    >> Set.ofSeq
    >> Set.count

let countAllMatches : string -> int =
    fun s -> s.Split nl 
    >> Array.map Set.ofSeq
    >> Set.intersectMany
    >> Set.count

Array.sumBy countAnyMatches input
|> printfn "Part 1: %A"

Array.sumBy countAllMatches input
|> printfn "Part 2: %A"
#time