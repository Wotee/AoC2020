#nowarn "0025"
open System
open type System.Environment
let input = System.IO.File.ReadAllText "inputs/day16.txt" |> fun s -> s.Split(NewLine + NewLine)
#time

let splitToLen x array = 
    let parts = Array.length array / x
    Array.splitInto parts array

let namedRules = 
    input.[0].Split([|NewLine; " or "; "-"; ": "|], StringSplitOptions.RemoveEmptyEntries)
    |> splitToLen 5
    |> Array.map (fun [|name;low1;high1;low2;high2|] ->
        name, fun x -> (int low1) <= x && x <= (int high1) || (int low2) <= x && x <= (int high2))

let allRules = 
    namedRules |> Array.map snd |> Array.reduce (fun acc elem -> fun x -> acc x || elem x)

let myTicket =
    input.[1].Split([|NewLine; ","; "your ticket:"|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let nearbyTickets =
    input.[2].Split([|NewLine; "nearby tickets:"|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Split(',') |> Array.map int)

let allTickets = Array.append [|myTicket|] nearbyTickets

let validTickets, invalidTickets = allTickets |> Array.partition (Array.forall allRules)

invalidTickets
|> Array.collect (Array.filter (allRules >> not))
|> Array.sum
|> printfn "Part 1: %i"

let getColumns (array : _ [] []) =
    let array2d = array2D array
    let cols = (Array.length (Array.head array))
    Array.init cols (fun c -> array2d.[*,c])

let filterIndex idx = Array.indexed >> Array.filter (fun (i,_) -> i <> idx) >> Array.map snd

let rec handleRules rules columns mem =
    Array.indexed columns
    |> Array.tryPick (fun (ci, col) ->
        Array.indexed rules
        |> Array.filter (fun (_, (_, rule)) -> Array.forall rule col)
        |> Array.tryExactlyOne // Most of columns have more than one matches here
        |> Option.map (fun (ri, (name, rule)) -> ci, ri, name, Array.head col))
    |> function
        | Some (colIdx, ruleIdx, ruleName, ticketValue) -> handleRules (filterIndex ruleIdx rules) (filterIndex colIdx columns) (Map.add ruleName ticketValue mem)
        | None -> mem

handleRules namedRules (getColumns validTickets) Map.empty
|> Map.toSeq
|> Seq.choose (fun (rule, value) -> if rule.StartsWith("departure") then Some (int64 value) else None)
|> Seq.reduce (*)
|> printfn "Part 2: %i"
