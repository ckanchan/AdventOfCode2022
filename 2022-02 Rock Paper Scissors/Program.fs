open System
type RockPaperScissors =
    | Rock
    | Paper
    | Scissors

let score rockPaperScissor =
    match rockPaperScissor with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

type Outcome = Win | Lose | Draw

let outcomeScore outcome = 
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let outcome opponent player =
    match (opponent, player) with
    | (Paper, Rock) -> Lose
    | (Scissors, Rock) -> Win
    | (Rock, Scissors) -> Lose
    | (Paper, Scissors) -> Win
    | (Scissors, Paper) -> Lose
    | (Rock, Paper) -> Win
    | (_, _) -> Draw

let inline (^) (opponent: RockPaperScissors)(player: RockPaperScissors) =
    outcome opponent player

let playerScore opponent player =
    let itemScore = score player
    let matchScore = ( opponent |> outcome <| player ) |> outcomeScore
    itemScore + matchScore

let RockPaperScissorsFromString str =
    match str with
        | "A" -> Some Rock 
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | "X" -> Some Rock
        | "Y" -> Some Paper
        | "Z" -> Some Scissors
        | _ -> None

let gameScoreFromLine (str: string) =
    let unwrapGame (arr: RockPaperScissors option[]) =
        match (arr[0], arr[1]) with
        | Some o, Some p -> Some(playerScore o p)
        | (_, _) -> None

    str.Split(" ") 
    |> Array.map RockPaperScissorsFromString 
    |> unwrapGame

let strategyTotal: seq<string> -> int = Seq.choose gameScoreFromLine >> Seq.sum

let testData = IO.File.ReadLines "./test"
let inputData = IO.File.ReadLines "./input"



let result = strategyTotal inputData

printfn "%d" result