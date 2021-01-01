let input = System.IO.File.ReadAllLines "inputs/day17.txt"
#time
type Cell =
    | Active
    | Inactive
    with static member ofChar = function '.' -> Inactive | '#' -> Active | _ -> failwith "Invalid data"

let rounds = 6
let possibleGrowth = 2 * rounds
let x = Array.length input + possibleGrowth
let y = Seq.length (Array.head input) + possibleGrowth
let z = 1 + possibleGrowth
let w = 1 + possibleGrowth
let initialData = input |> Array.map (Seq.toArray >> Array.map Cell.ofChar)
let middleZ = z / 2
let middleW = w / 2

let initializer3D x' y' z' =
    if z' = middleZ && rounds <= x' && x' < (x - rounds) && rounds <= y' && y' < (y - rounds)
    then initialData.[x'-rounds].[y'-rounds]
    else Inactive

let initializer4D x' y' z' w' =
    if z' = middleZ && w' = middleW && rounds <= x' && x' < (x - rounds) && rounds <= y' && y' < (y - rounds)
    then initialData.[x'-rounds].[y'-rounds]
    else Inactive

let values v = [|v-1..v+1|]

let getAdjacentCoordinates3d x y z =
    Array.allPairs (Array.allPairs (values x) (values y)) (values z)
    |> Array.choose (fun ((x', y'), z') -> if x = x' && y = y' && z = z' then None else Some (x', y', z'))

let getAdjacentCoordinates4d x y z w =
    Array.allPairs (Array.allPairs (Array.allPairs (values x) (values y)) (values z)) (values w)
    |> Array.choose (fun (((x', y'), z'), w') -> if x = x' && y = y' && z = z' && w = w' then None else Some (x', y', z', w'))

let itemOrDefault3d (cell : _[,,]) (x, y, z) = 
    try cell.[x,y,z] with | _ -> Inactive

let itemOrDefault4d (cell : _[,,,]) (x, y, z, w) =
    try cell.[x,y,z,w] with | _ -> Inactive

let countAdjacent3d x y z cell =
    getAdjacentCoordinates3d x y z
    |> Array.sumBy (itemOrDefault3d cell >> function Active -> 1 | _ -> 0)

let countAdjacent4d x y z w cell =
    getAdjacentCoordinates4d x y z w
    |> Array.sumBy (itemOrDefault4d cell >> function Active -> 1 | _ -> 0)

let determineState = function | Active, 2 | Active, 3 | Inactive, 3 -> Active | _, _ -> Inactive

let countNext3d (current : Cell [,,]) =
    Array3D.init (Array3D.length1 current) (Array3D.length2 current) (Array3D.length3 current) (fun x y z -> determineState (current.[x,y,z], countAdjacent3d x y z current))

let countNext4d (current : Cell [,,,]) =
    Array4D.init (Array4D.length1 current) (Array4D.length2 current) (Array4D.length3 current) (Array4D.length4 current) (fun x y z w -> determineState (current.[x,y,z,w], (countAdjacent4d x y z w current)))

let roundValues countNext = Seq.unfold (fun s0 -> Some (s0, countNext s0))

let run countNext rounds =
   roundValues countNext 
   >> Seq.item rounds
   >> Seq.cast<Cell>
   >> Seq.sumBy (function Active -> 1 | Inactive -> 0)

Array3D.init x y z initializer3D
|> run countNext3d rounds
|> printfn "Part 1: %i"

Array4D.init x y z w initializer4D
|> run countNext4d rounds
|> printfn "Part 2: %i"