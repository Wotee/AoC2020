let input = System.IO.File.ReadAllLines "inputs/day2.txt"
#time

type PasswordRule = 
    {
        lower : int
        upper : int
        character : char
        password : string
    }

let parse (row : string) = 
    let values = row.Split [|' ';'-'|]
    {
        lower = values.[0] |> int
        upper = values.[1] |> int
        character = values.[2].[0] // This feels hacky, but we only need the first char here
        password = values.[3]
    }

let passwordRules = 
    input |> Array.map parse

let task1 ({lower = lower; upper = upper; character = character; password = password}) =
    let count = password |> Seq.filter ((=) character) |> Seq.length
    lower <= count && count <= upper

let task2 ({lower = lower; upper = upper; character = character; password = password}) = 
    (password.[lower-1] = character) <> (password.[upper-1] = character)

let count filter = Array.filter filter >> Array.length

passwordRules |> count task1 |> printfn "Part 1: %A"
passwordRules |> count task2 |> printfn "Part 2: %A"
#time