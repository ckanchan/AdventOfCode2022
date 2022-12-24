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
    static member shortTest = [NoOp; AddX 3; AddX -5] |> Seq.ofList

type CPUState = 
    {registerX: int; cycle: int}
    static member initial = {registerX=1;cycle=1}
    member cpu.signalStrength = cpu.registerX * cpu.cycle

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

let processInstruction (cpuState: CPUState) instruction =
    match instruction with
    | NoOp ->
         Array.singleton {cpuState with cycle = cpuState.cycle + 1}
    | AddX value ->
        [|
            {cpuState with cycle = (cpuState.cycle + 1)};
            {registerX=cpuState.registerX + value; cycle = cpuState.cycle + 2}
        |]

let processInstructionL (cpuStates: CPUState array) instruction =
    let state = Array.last cpuStates 
    processInstruction state instruction

let signalStrength cpuState =
    let currentCycle = cpuState.cycle - 1
    currentCycle * cpuState.registerX

let cpuStatesFromInstructions (instructions: CPUInstruction seq) =
    instructions
    |> Seq.scan processInstructionL [|CPUState.initial|]
    |> Array.concat

let getInterestingSignals (cpuStates: CPUState array) =
    let decrement i = i - 1
    let interestingSignalIndices = [|20..40..220|] |> Array.map decrement
    interestingSignalIndices 
    |> Array.choose (fun i -> Array.tryItem  i cpuStates)
    |> Array.map (fun s -> {|x=s.registerX;cycle=s.cycle; signalStrength = s.signalStrength|})


let test = testData |> Seq.choose cpuInstructionFromString |> cpuStatesFromInstructions

test
|> getInterestingSignals
|> Array.sumBy (fun x -> x.signalStrength)
|> printfn "%d"