open Microsoft.FSharp.Collections
let input = System.IO.File.ReadAllLines("inputs/day11.txt")

#time
type Position = Floor | Empty | Occupied with
    static member ofChar = function '.' -> Floor | 'L' -> Empty | '#' -> Occupied | _ -> failwith ""
    member x.isOccupied = match x with Occupied -> true | _ -> false
    member x.isEmpty = match x with Empty -> true | _ -> false

let map = 
    input
    |> Array.map (Seq.toArray >> Array.map Position.ofChar)
    |> array2D

let tryGet (map : Position[,]) (x,y) = try Some map.[x, y] with | _ -> None

let getAdjacentPart1 x y map = 
    Array.allPairs [|x-1..x+1|] [|y-1..y+1|]
    |> Array.except [x, y]
    |> Array.choose (tryGet map)

let getAdjacentPart2 x y map =
    let rec nextSeatInDir (pos : int*int) (dir : int*int -> int*int) =
        let newPos = dir pos
        match tryGet map newPos with
        | Some Floor -> nextSeatInDir newPos dir
        | x -> x
    [| fun (x,y) -> x+1,y-1
       fun (x,y) -> x+1,y
       fun (x,y) -> x+1,y+1
       fun (x,y) -> x,y-1
       fun (x,y) -> x,y+1
       fun (x,y) -> x-1,y-1
       fun (x,y) -> x-1,y
       fun (x,y) -> x-1,y+1 |]
    |> Array.choose (nextSeatInDir (x,y))

let (|BecomesOccupied|_|) (adjacent : Position array) =
    match adjacent |> Array.exists (fun seat -> seat.isOccupied) with
    | true -> None
    | false -> Some BecomesOccupied

let (|BecomesEmpty|_|) limit (adjacent : Position array) = 
    let value = adjacent |> Array.filter (fun seat -> seat.isOccupied) |> Seq.length
    match value with
    | x when x >= limit -> Some BecomesEmpty
    | _ -> None

let handleRounds (map : Position [,]) getter limit =
    let rec handleRound map =
        let mapCopy = Array2D.copy map

        map
        |> Array2D.iteri (fun x y (elem : Position) ->
            match getter x y map with
            | BecomesOccupied when elem.isEmpty -> mapCopy.[x,y] <- Occupied
            | BecomesEmpty limit when elem.isOccupied -> mapCopy.[x,y] <- Empty
            | _ -> mapCopy.[x,y] <- elem)

        if map = mapCopy
        then mapCopy
        else handleRound mapCopy
    handleRound map

let countOccupied : Position[,] -> int =
    Seq.cast<Position>
    >> Seq.filter (fun x -> x.isOccupied)
    >> Seq.length

handleRounds map getAdjacentPart1 4
|> countOccupied
|> printfn "Part 1: %i"

handleRounds map getAdjacentPart2 5
|> countOccupied
|> printfn "Part 2: %i"
