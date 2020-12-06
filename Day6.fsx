let input = System.IO.File.ReadAllText "inputs/day6.txt" |> fun s -> s.Split $"{System.Environment.NewLine}{System.Environment.NewLine}"

#time
let countAnyMatches : string -> int =
    fun s -> s.Replace(System.Environment.NewLine, "")
    >> Set.ofSeq
    >> Set.count

let countAllMatches : string -> int =
    fun s -> s.Split(System.Environment.NewLine) 
    >> Array.map Set.ofSeq
    >> Set.intersectMany
    >> Set.count

Array.sumBy countAnyMatches input
|> printfn "Part 1: %A"

Array.sumBy countAllMatches input
|> printfn "Part 2: %A"
#time