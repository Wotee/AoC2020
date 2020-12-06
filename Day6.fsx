let input = System.IO.File.ReadAllText "inputs/day6.txt" |> fun s -> s.Split $"{System.Environment.NewLine}{System.Environment.NewLine}"

#time
let run f = Array.map f >> Array.sum

let countAnyMatches (s : string) = s.Replace(System.Environment.NewLine, "") |> Set.ofSeq |> Set.count
let countAllMatches (s : string) = s.Split(System.Environment.NewLine) |> Array.map Set.ofSeq |> Set.intersectMany |> Set.count

run countAnyMatches input
|> printfn "Part 1: %A"

run countAllMatches input
|> printfn "Part 2: %A"
#time