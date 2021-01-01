let input = System.IO.File.ReadAllLines "inputs/day17.txt"
#time
type Cell =
    | Active
    | Inactive
    with static member ofChar = function '.' -> Inactive | '#' -> Active | _ -> failwith "Invalid data"

let rounds = 6
let initialData = input |> Array.map (Seq.toArray >> Array.map Cell.ofChar)
let x = Array.length initialData
let y = Array.length (Array.head initialData)

let valid value maxVal = 0 <= value && value < maxVal

let getItemOrDefault3d x y z (array : Cell [,,]) =
    if (valid x (Array3D.length1 array)) && (valid y (Array3D.length2 array)) && (valid z (Array3D.length3 array))
    then array.[x,y,z]
    else Inactive

let getItemOrDefault4d x y z w (array : Cell [,,,]) =
    if (valid x (Array4D.length1 array)) && (valid y (Array4D.length2 array)) && (valid z (Array4D.length3 array)) && (valid w (Array4D.length4 array))
    then array.[x,y,z,w]
    else Inactive

let countAdjacent3d x y z array =
    [0..26] // (3^3) - 1, because 3D space
    |> List.filter ((<>) 13) // (0,0,0)
    |> List.sumBy (fun i ->
        match x + i % 3 - 1, y + (i / 3) % 3 - 1, z + i / 9 - 1 with
        | x', y', z' when
            (valid x' (Array3D.length1 array)) &&
            (valid y' (Array3D.length2 array)) &&
            (valid z' (Array3D.length3 array)) ->
            match array.[x', y', z'] with
            | Active -> 1
            | Inactive -> 0
        | _,_,_ -> 0
    )

let countAdjacent4d x y z w array =
    [0..80] // (3^4) - 1, because 4D space
    |> List.filter ((<>) 40) // (0,0,0,0)
    |> List.sumBy (fun i ->
        match x + i % 3 - 1, y + ((i / 3) % 3 - 1), z + ((i / 9) % 3 - 1), w + (i / 27 - 1) with
        | x', y', z', w' when
            (valid x' (Array4D.length1 array)) &&
            (valid y' (Array4D.length2 array)) &&
            (valid z' (Array4D.length3 array)) &&
            (valid w' (Array4D.length4 array)) ->
                match array.[x', y', z', w'] with
                | Active -> 1
                | Inactive -> 0
        | _,_,_,_ -> 0
    )

let determineNewState = function | Active, 2 | Active, 3 | Inactive, 3 -> Active | _, _ -> Inactive

let countNext4d (current : _ [,,,]) =
    Array4D.init ((Array4D.length1 current) + 2) ((Array4D.length2 current) + 2) ((Array4D.length3 current) + 2) ((Array4D.length4 current) + 2)
        (fun x y z w -> determineNewState (getItemOrDefault4d (x-1) (y-1) (z-1) (w-1) current, (countAdjacent4d (x-1) (y-1) (z-1) (w-1) current)))

let countNext3d (current : _ [,,]) =
    Array3D.init ((Array3D.length1 current) + 2) ((Array3D.length2 current) + 2) ((Array3D.length3 current) + 2)
        (fun x y z -> determineNewState (getItemOrDefault3d (x-1) (y-1) (z-1) current, (countAdjacent3d (x-1) (y-1) (z-1) current)))

let roundValues countNext = Seq.unfold (fun s0 -> Some (s0, countNext s0))

let run countNext rounds =
   roundValues countNext 
   >> Seq.item rounds
   >> Seq.cast<Cell>
   >> Seq.sumBy (function Active -> 1 | Inactive -> 0)

Array3D.init x y 1 (fun x y _ -> initialData.[x].[y])
|> run countNext3d rounds
|> printfn "Part 1: %i"

Array4D.init x y 1 1 (fun x y _ _ -> initialData.[x].[y])
|> run countNext4d rounds
|> printfn "Part 2: %i"
