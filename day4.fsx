let extractParts (text:string) (separator:char) =
    let split = text.Split(separator)
    split.[0], split.[1]

let parseLine (line:string) =
    let parseRange range =
        let start, stop = extractParts range '-'
        [int start .. int stop]
 
    let first, second = extractParts line ','

    [parseRange first; parseRange second]

let overlaps sets =
    sets 
    |> Seq.pairwise 
    |> Seq.map (fun (s1, s2) -> (Set.isSubset s1 s2) || (Set.isSuperset s1 s2))
    |> Seq.reduce (fun acc b -> acc && b) 


let inputPath = "inputs/day4.txt"
let totalPart1 = System.IO.File.ReadLines inputPath
                |> Seq.map parseLine
                |> Seq.map (fun ranges -> ranges |> Seq.map Set.ofList)
                |> Seq.map overlaps
                |> Seq.map (fun b -> if b then 1 else 0)
                |> Seq.sum

printfn "The number of overlaps for part1 is: %i" totalPart1

let totalPart2 = System.IO.File.ReadLines inputPath
                |> Seq.map parseLine
                |> Seq.map (fun ranges -> ranges |> Seq.map Set.ofList)
                |> Seq.map Set.intersectMany
                |> Seq.map Set.isEmpty
                |> Seq.map (fun b -> if b then 0 else 1)
                |> Seq.sum

printfn "The number of overlaps for part2 is: %i" totalPart2