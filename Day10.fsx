open System.Collections.Generic
let input = System.IO.File.ReadAllLines("inputs/day10.txt") |> Array.map int

let joltages =
    input
    |> Array.append [|0; Array.max input + 3|]
    |> Array.sort

joltages
|> Array.pairwise
|> Array.groupBy (fun (a,b) -> b - a)
|> Array.map (fun (_, value) -> Array.length value)
|> Array.reduce (*)
|> printfn "Part 1: %i" 

let tryGet (routes : Dictionary<_, _>) = routes.TryGetValue >> snd

let tribonacci (routes : Dictionary<_,_>) i =
    tryGet routes (i - 1) + tryGet routes (i - 2) + tryGet routes (i - 3)

let routes = Dictionary<int, int64>()
routes.Add(0, 1L)

joltages
|> Array.tail
|> Array.iter (fun jolt -> routes.[jolt] <- tribonacci routes jolt)

routes.[Array.max joltages] |> printfn "Part 2: %i"