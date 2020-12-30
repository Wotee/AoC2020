#nowarn "0025"
let input = System.IO.File.ReadAllLines("inputs/day09.txt") |> Array.map int64

#time
let preamble = 25

let isValid : int64 array -> bool = 
    Array.toList
    >> List.rev
    >> fun (head::tail) -> List.allPairs tail tail |> List.exists (fun (a,b) -> a + b = head)

let part1 =
    input
    |> Seq.windowed (preamble + 1)
    |> Seq.find (isValid >> not)
    |> Seq.last

part1
|> printfn "Part 1: %i"

let part2input =
    Array.splitAt (Array.findIndex ((=) part1) input) input |> fst

let allWindows input = 
    seq {2..Array.length input}
    |> Seq.collect (fun len -> Array.windowed len input)

part2input
|> allWindows
|> Seq.find (Array.sum >> (=) part1)
|> fun arr -> Array.max arr + Array.min arr
|> printfn "Part 2: %i"
#time