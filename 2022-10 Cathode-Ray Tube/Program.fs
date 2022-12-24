open System.IO

let testData = File.ReadLines("./test")
let inputData = File.ReadLines("./input")

let tryParseInt (str: string) =
    match System.Int32.TryParse(str) with
    | (true, number) -> Some number
    | (_, _) -> None

type CPUInstruction =
    | NoOp
    | AddX of int

let cpuInstructionFromString (str: string) =
    let elements = str.Split(" ")
    match elements[0] with
    | "noop" -> 
        Some NoOp
    | "addx" ->
        match tryParseInt(elements[1]) with
        | Some number -> AddX number |> Some
        | None -> None
    | _ -> 
        None