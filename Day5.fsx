let input = System.IO.File.ReadAllLines "inputs/day5.txt"

#time
let splitBinarySearch (lower, upper) =
    let mid = (lower + upper) / 2
    function 'F' | 'L' -> lower, mid | _ -> mid+1, upper

let parse upperLimit input =
    Array.fold splitBinarySearch (0, upperLimit) input
    |> fst

let splitDirections : string -> char array * char array =
    Seq.toArray >> Array.splitAt 7

let seatIds =
    input
    |> Array.map (
        splitDirections
        >> (fun (rowInput, seatInput) -> parse 127 rowInput, parse 7 seatInput)
        >> (fun (row, seat) -> row * 8 + seat))
    |> Array.sort

let max = Array.last seatIds

max
|> printfn "Part 1: %A"

[| Array.head seatIds .. max |]
|> Array.except seatIds
|> Array.exactlyOne
|> printfn "Part 2: %A"