#nowarn "0025"
let input = System.IO.File.ReadAllLines "inputs/day14.txt"
#time

let toNum value = System.Convert.ToUInt64(value, 2)
let fromNum (num : int) = System.Convert.ToString(num, 2).PadLeft(36, '0')

let (|Mask|Memory|) (x:string) = 
    let split = x.Split(" = ")
    match split.[0] with
    | "mask" -> Mask split.[1]
    | memory -> Memory ((int memory.[4..^1]), fromNum (int split.[1]))

let applyPart1Mask value mask = 
    Seq.zip value mask
    |> Seq.map (fun (v, m) -> match m with 'X' -> v | a -> a)
    |> System.String.Concat

let applyPart2Mask address mask =
    let update idx v =
        Array.mapi (fun i x -> if i = idx then v else x)

    let maskedAddress =
        Seq.zip (fromNum address) mask
        |> Seq.map (fun (a, m) -> match m with | '0' -> a | v -> v)
        |> Seq.toArray

    let rec findAddressVariants (current::rest) = 
        match Array.tryFindIndex ((=) 'X') current with
        | None -> current::rest
        | Some ix ->
            let newVariants = current |> fun c -> rest @ [update ix '0' c; update ix '1' c]
            findAddressVariants newVariants
    
    findAddressVariants [maskedAddress]
    |> List.map (System.String.Concat >> toNum)

let part1Memory mem slot value mask = 
    mem |> Map.change slot (fun _ -> Some (applyPart1Mask value mask)), mask

let part2Memory mem slot value mask = 
    applyPart2Mask slot mask |> List.fold (fun (acc : Map<uint64, string>) (elem : uint64) -> acc |> Map.change elem (fun _ -> Some value)) mem, mask

let handle maskFunction (mem, mask) = function
    | Mask newMask -> (mem, newMask)
    | Memory (slot, value) -> maskFunction mem slot value mask

let run f = 
    Array.fold f (Map.empty, "") >> fst >> Seq.sumBy (fun item -> toNum item.Value)

run (handle part1Memory) input
|> printfn "Part 1: %i"

run (handle part2Memory) input
|> printfn "Part 2: %i"