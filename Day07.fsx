#nowarn "0025"
let input = System.IO.File.ReadAllLines("inputs/day07.txt") |> Array.toList
#time

let parseLine (line : string) = 
    line.Split ([|" contain "; " bags"; " bag"; "."; ", "; "no other"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> fun (head::tail) ->
        tail
        |> List.collect (fun color -> List.replicate (int (System.Char.GetNumericValue color.[0])) color.[2..])
        |> fun newTail -> head::newTail

let rules = List.map parseLine input 

let rec canContain color = 
    let result = rules |> List.filter (List.tail >> List.exists ((=) color)) |> List.map List.head
    result @ (result |> List.collect canContain) |> List.distinct

let rec mustContain color =
    let result = rules |> List.find (List.head >> (=) color) |> List.tail
    result @ (result |> List.collect mustContain)

"shiny gold" |> canContain |> List.length |> printfn "%i"
"shiny gold" |> mustContain |> List.length |> printfn "%i"
