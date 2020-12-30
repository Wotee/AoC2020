let input = System.IO.File.ReadAllLines("inputs/day13.txt")
#time

let part1 = 
    let timeStamp = int input.[0]
    let busLines = input.[1] |> fun s -> s.Split([|'x'; ','|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    let departuresAfterTimeStamp = 
        busLines
        |> Array.map (fun line -> line, line * (timeStamp / line + 1))

    departuresAfterTimeStamp
    |> Array.minBy snd
    |> fun (line, earliestDeparture) -> line * (earliestDeparture - timeStamp)

printfn "Part 1: %i" part1

let part2 = 
    let lcm : (int64 * int64) array -> int64 = Array.map snd >> Array.reduce (*)
    let timeStamp = 100000000000000L
    let busLines =
        input.[1]
        |> fun s -> s.Split(',')
        |> Array.indexed
        |> Array.choose (fun (i, x) -> if x = "x" then None else Some (int64 i, int64 x))
    let count = Array.length busLines - 1
    
    let rec testStamp index increment stamp =
        let result = busLines.[..index] |> Seq.forall (fun (i, id) -> (stamp + i) % id = 0L)
        match result, index = count with 
        | false, _ -> testStamp index increment (stamp + increment)
        | true, false -> testStamp (index+1) (lcm busLines.[..index]) stamp
        | true, true -> stamp

    testStamp 0 1L timeStamp

printfn "Part 2: %i" part2