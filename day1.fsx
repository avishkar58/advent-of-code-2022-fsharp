let accumulator acc line =
    let list, counter = acc
    match line with
    | "" -> counter::list, 0
    | _ -> list, counter + int line

let calcTotals lines = 
    let result, _ = lines |> Seq.fold accumulator ([], 0)
    result
    
let inputPath = "inputs/day1_1.txt"
let sortedList = System.IO.File.ReadLines inputPath |> calcTotals |> List.sortDescending

// Part 1
let maxCals = sortedList.Head
printfn "Maximum calories: %i" maxCals

// Part 2
let topThreeCals = List.take 3 sortedList |> List.sum
printfn "Sum of top 3 calories: %i" topThreeCals
