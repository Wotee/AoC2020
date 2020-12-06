let input = System.IO.File.ReadAllLines "inputs/day1.txt" |> Array.map int

#time
let makePairs inp = List.allPairs inp inp

let len = Array.length input - 1
let pairs = [|for i in 0..len do for j in 1..len do yield (input.[i], input.[j])|]

pairs
|> Array.pick (fun (x, y) -> if x + y = 2020 then Some (x * y) else None)
|> printfn "Part 1: %A"

input
|> Array.pick (fun i -> pairs |> Array.tryPick (fun (x, y) -> if i + x + y = 2020 then Some (x*y*i) else None))
|> printfn "Part 2: %A"
#time