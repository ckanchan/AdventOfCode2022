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

let OutcomeFromString str =
    match str with
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None

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

let computeObjectToPlayForOutcome opponent outcome =
    match (outcome, opponent) with
    | (Win, Rock) -> Paper
    | (Win, Paper) -> Scissors
    | (Win, Scissors) -> Rock
    | (Lose, Rock) -> Scissors
    | (Lose, Paper) -> Rock
    | (Lose, Scissors) -> Paper
    | (Draw, _) -> opponent


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

let gameScoreFromLine2 (str: string) =
    let elems = str.Split(" ")
    let opponent = RockPaperScissorsFromString elems[0]
    let outcome = OutcomeFromString elems[1]
    
    match (opponent, outcome) with
        | Some opponent, Some outcome -> 
            computeObjectToPlayForOutcome opponent outcome 
            |> playerScore opponent 
            |> Some
        | (_, _) -> None

let strategyTotal2: string seq -> int = Seq.choose gameScoreFromLine2 >> Seq.sum

let testData = IO.File.ReadLines "./test"
let inputData = IO.File.ReadLines "./input"





let result = strategyTotal2 testData

printfn "%d" result