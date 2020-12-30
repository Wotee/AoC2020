#nowarn "0025"
let input = System.IO.File.ReadAllLines("inputs/day08.txt")

#time
type OperationType = 
    | Acc of int
    | Jmp of int
    | Nop of int
    static member OfString (s : string) =
        match s.Split " " with
        | [|"acc"; value|] -> Acc (int value)
        | [|"jmp"; value|] -> Jmp (int value)
        | [|"nop"; value|] -> Nop (int value)
        | _ -> failwith "Incorrect instruction format"

type Operation =
    { Operation : OperationType
      Visited : bool }
    static member Create (s : string) =
        { Operation = OperationType.OfString s; Visited = false }
    static member Visit (x : Operation) =
        { x with Visited = true }
        
type ExecutionStatus = Loop of int | Completed of int

let instructions = 
    input
    |> Array.mapi (fun i x -> i, Operation.Create x)
    |> Map.ofArray

let execute instructions = 
    let rec executeInstructions i acc (instructions : Map<int, Operation>) =
        match Map.tryFind i instructions with
        | None -> Completed acc
        | Some instruction when instruction.Visited -> Loop acc
        | Some current ->
            instructions
            |> Map.change i (Option.map Operation.Visit)
            |> (match current.Operation with
                | Nop _ -> executeInstructions (i + 1) acc
                | Acc value -> executeInstructions (i + 1) (acc + value)
                | Jmp value -> executeInstructions (i + value) acc
            )
    executeInstructions 0 0 instructions

execute instructions
|> fun (Loop value) -> printfn "Part 1: %A" value

let fixInstructions (instructions : Map<int, Operation>) =
    Seq.replicate (instructions.Count) instructions
    |> Seq.indexed
    |> Seq.choose (fun (i, map) ->
        let current = Map.find i map
        match current.Operation with
        | Nop value -> Map.change i (Option.map (fun _ -> {current with Operation = Jmp value})) map |> Some
        | Jmp value -> Map.change i (Option.map (fun _ -> {current with Operation = Nop value})) map |> Some
        | Acc _ -> None)
    
instructions
|> fixInstructions
|> Seq.pick (fun alteredInstructions -> match execute alteredInstructions with Completed acc -> Some acc | Loop _ -> None)
|> printfn "Part 2: %A"
#time