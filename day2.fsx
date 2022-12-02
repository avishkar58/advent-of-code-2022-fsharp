let score (player1, player2) = 
    let modResult = (player2 - player1 + 1) % 3
    // Fix for weird modulus sign behaviour in F#
    let result = if modResult >= 0 then modResult else 3 + modResult
    let score =  result * 3 + player2
    score

let mapP1 player1 = 
    match player1 with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3
    | _ -> 0

let decryptPart1 (player1, player2) = 
    let p2 = match player2 with
             | "X" -> 1  // Rock
             | "Y" -> 2  // Paper
             | "Z" -> 3  // Scissors
             | _ -> 0
    
    mapP1 player1, p2

let decryptPart2 (player1, player2) = 
    let p1 = mapP1 player1
    let p2 = match player2 with
                  | "X" -> (p1 + 1) % 3 + 1 // Lose
                  | "Y" -> p1  // Draw
                  | "Z" -> (p1 + 1) % 3
                  | _ -> 0
    p1, p2

let inputPath = "inputs/day2.txt"
let totalPart1 = System.IO.File.ReadLines inputPath 
                 |> Seq.map (fun l -> l.Split(" ")) 
                 |> Seq.map(fun s -> s[0], s[1]) 
                 |> Seq.map decryptPart1
                 |> Seq.map score
                 |> Seq.sum

let totalPart2 = System.IO.File.ReadLines inputPath 
                 |> Seq.map (fun l -> l.Split(" ")) 
                 |> Seq.map(fun s -> s[0], s[1]) 
                 |> Seq.map decryptPart2
                 |> Seq.map score
                 |> Seq.sum

printfn "Total Score for Part 1: %i" totalPart1
printfn "Total Score for Part 2: %i" totalPart2

