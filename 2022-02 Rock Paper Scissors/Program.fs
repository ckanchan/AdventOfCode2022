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

let playerScore opponent player =
    let itemScore = score player
    let matchScore = outcome opponent player |> outcomeScore
    itemScore + matchScore
