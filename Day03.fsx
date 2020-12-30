let input = System.IO.File.ReadAllLines "inputs/day03.txt"

#time
let splittedInput = 
    input |> Array.map Seq.toArray

// Simulate "Donut" data structure
let getItem index list = 
    list |> Array.item (index % (list |> Array.length))

let getAllLocationsWithMove dx dy = 
    let rowAmount = Array.length input
    let rec getLocations x y = [| yield splittedInput |> getItem y |> getItem x; if y+dy < rowAmount then yield! getLocations (x + dx) (y + dy) |]
    getLocations dx dy

let countChar x = Array.filter ((=) x) >> Array.length

getAllLocationsWithMove 3 1
|> countChar '#'
|> printfn "Part 1: %A"

// You could skip (3,1) and use precalculated part 1, but I wont do it now
[|(1,1);(3,1);(5,1);(7,1);(1,2)|]
|> Array.map (fun (dx, dy) -> getAllLocationsWithMove dx dy |> countChar '#' |> int64)
|> Array.reduce (*)
|> printfn "Part 2: %A"
#time