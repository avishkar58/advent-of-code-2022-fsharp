let split (line: string) =
    let half = line.Length / 2
    [line.[..half-1]; line.[half..]]

let intersect lines = 
    lines |> Seq.map Set.ofSeq |> Seq.reduce Set.intersect |> Set.maxElement

let getValue (c:char) =
    if c >= 'a' then int c - 96 else int c - 38

let chunk windowSize seq =
    let folder acc item =
        let result, current = acc
        if List.length current = (windowSize - 1) then result @ [current @ [item]], [] else result, current @ [item]

    let result, _ = seq |> Seq.fold folder ([], [])
    result

let inputPath = "inputs/day3.txt"

// Part 1
let totalPart1 = System.IO.File.ReadLines inputPath
                |> Seq.map split
                |> Seq.map intersect
                |> Seq.map getValue
                |> Seq.sum

printfn "Total value of common items: %i" totalPart1

// Part 2
let totalPart2 = System.IO.File.ReadLines inputPath
                |> chunk 3
                |> Seq.map intersect
                |> Seq.map getValue
                |> Seq.sum

printfn "Total value of badges: %i" totalPart2
