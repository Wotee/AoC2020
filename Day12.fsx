#nowarn "0025"
let input = System.IO.File.ReadAllLines("inputs/day12.txt")
#time

type Orientation =
    | North | East | South | West
    with
    static member OfNumValue x =
        match (x + 360) % 360 with | 0 -> North | 90 -> East | 180 -> South | 270 -> West | y -> failwithf "Incorrect value: %i, div %i" x y
    member x.ToNumValue =
        match x with | North -> 0 | East -> 90 | South -> 180 | West -> 270
    static member (+) (a : Orientation, b : int) = Orientation.OfNumValue (a.ToNumValue + b)
    static member (-) (a : Orientation, b : int) = Orientation.OfNumValue (a.ToNumValue - b)

type Waypoint =
    { xPos : int
      yPos : int }
    with
    static member Create =
        { xPos = 10; yPos = 1 }
    member x.RotateClockwise =
        {x with xPos = x.yPos; yPos = - x.xPos}
    member x.RotateCounterClockwise = 
        {x with xPos = - x.yPos; yPos = x.xPos}

type Ship =
    { xPos : int
      yPos : int
      Orientation : Orientation
      Waypoint : Waypoint }
    with
    static member Create =
        { xPos = 0; yPos = 0; Orientation = East; Waypoint = Waypoint.Create}

let commands =
    input
    |> Array.map (Seq.toList >> fun (char::amount) -> char, amount |> System.String.Concat |> int)

let part1 (acc : Ship) (elem : char*int) =
    match elem, acc.Orientation with
    | ('N', value), _ | ('F', value), North -> { acc with yPos = acc.yPos + value }
    | ('S', value), _ | ('F', value), South -> { acc with yPos = acc.yPos - value }
    | ('E', value), _ | ('F', value), East -> { acc with xPos = acc.xPos + value }
    | ('W', value), _ | ('F', value), West -> { acc with xPos = acc.xPos - value }
    | ('L', value), _ -> { acc with Orientation = acc.Orientation - value }
    | ('R', value), _ -> { acc with Orientation = acc.Orientation + value }
    | (x,_), _ -> failwithf "Unrecognized action: %c" x

let rotateNDegrees (value : int) (f : Waypoint -> Waypoint) (wp : Waypoint) =
    Seq.init (abs value / 90) (fun _ -> f) |> Seq.fold (fun acc elem -> elem acc) wp

let part2 (acc : Ship) (elem : char*int) =
    match elem with
    | 'N', value -> { acc with Waypoint = {acc.Waypoint with yPos = acc.Waypoint.yPos + value }}
    | 'S', value -> { acc with Waypoint = {acc.Waypoint with yPos = acc.Waypoint.yPos - value }}
    | 'E', value -> { acc with Waypoint = {acc.Waypoint with xPos = acc.Waypoint.xPos + value }}
    | 'W', value -> { acc with Waypoint = {acc.Waypoint with xPos = acc.Waypoint.xPos - value }}
    | 'L', value -> { acc with Waypoint = rotateNDegrees value (fun wp -> wp.RotateCounterClockwise) acc.Waypoint}
    | 'R', value -> { acc with Waypoint = rotateNDegrees value (fun wp -> wp.RotateClockwise) acc.Waypoint}
    | 'F', value -> { acc with xPos = acc.xPos + (acc.Waypoint.xPos * value); yPos = acc.yPos + (acc.Waypoint.yPos * value)}
    | x, _ -> failwithf "Unrecognized action: %c" x

let run f =
    commands
    |> Array.fold f Ship.Create
    |> fun ship -> abs ship.xPos + abs ship.yPos

run part1 |> printfn "Part 1: %i"
run part2 |> printfn "Part 2: %i"