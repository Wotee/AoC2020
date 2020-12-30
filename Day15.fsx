let input = System.IO.File.ReadAllText("inputs/day15.txt") |> fun s -> s.Split(',') |> Array.map int
#time
let len = Array.length input
let last = Array.last input

let mem = input |> Array.mapi (fun i x -> x, i+1) |> dict |> System.Collections.Generic.Dictionary

let values = 
    Seq.unfold (fun (s0, ix) ->
        let s1 =
            match mem.TryGetValue(s0) with
            | false, _ -> 0
            | true, value -> ix - value
        mem.[s0] <- ix
        Some ((s0, ix), (s1, ix+1))
    ) (last, len)
    |> Seq.cache

let runToIndex index =
    Seq.skipWhile (fun (_, idx) -> idx < index) >> Seq.head >> fst

runToIndex 2020 values
|> printfn "Part 1: %i"

runToIndex 30000000 values
|> printfn "Part 2: %i"