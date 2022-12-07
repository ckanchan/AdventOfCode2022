#if INTERACTIVE
#r "nuget: FSharp.Text.RegexProvider"
#endif

open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions


let testData = File.ReadLines "./test"
let inputData = File.ReadLines "./input"

type InstructionMatcher = FSharp.Text.RegexProvider.Regex< @"move (?'itemCount'\d+) from (?'source'\d+) to (?'destination'\d+)" >

let generateCurrentStackPositions (lines: string seq) =
    let generateStackMatchString count = 
        let singleMatchPattern = sprintf @"(?:\[|\s)(?'stack%d'[A-Z]|\s)(?:\s|\])"
        [1..count]
        |> List.map (fun (number) -> singleMatchPattern number)
        |> String.concat " "

    let currentPositionString = lines |> Seq.takeWhile (fun line -> line <> "")
    let maximumCount = 
        currentPositionString 
        |> Seq.last
        |> Seq.map string
        |> Seq.filter (fun x -> x <> " ")
        |> Seq.map int
        |> Seq.max

    let getCapturedGroups (matches: System.Text.RegularExpressions.MatchCollection) =
        let getValue(group: System.Text.RegularExpressions.Group) =
            group.Value

        matches 
        |> Seq.map (fun x -> x.Groups |> Seq.tail |> Seq.map getValue)
        |> Seq.filter (fun x -> not <|Seq.isEmpty x)
    
    let matcherString = generateStackMatchString maximumCount
    let matcher = System.Text.RegularExpressions.Regex(matcherString)
    let matchedLines = 
        lines 
        |> Seq.map (fun x -> matcher.Matches(x))
        |> Seq.collect getCapturedGroups
        |> Seq.transpose
        |> Seq.map (fun x -> x |> Seq.filter (fun s -> s <> " "))
        |> Seq.map (List.ofSeq)
        |> Array.ofSeq
    
    matchedLines
    
let rearrangeCrates inputData =
    let initialState = generateCurrentStackPositions inputData
    let rec moveCrates stackState numberOfCrates fromStack toStack =
        let mut updatedStackState: string list[] = Array.copy stackState
        let moveCrateFrom (sourceStack: string list) (targetStack: string list) =
            let removedCrate ::restOfSource = sourceStack
            let resultTargetStack = removedCrate::targetStack
            (restOfSource, resultTargetStack)
        
        let sourceStack = stackState[fromStack]
        let targetStack = stackState[toStack]

        let (resultSource, resultTarget) = moveCrateFrom sourceStack targetStack
        Array.set stackState fromStack resultSource
        Array.set stackState toStack resultTarget
        
        let newCount = numberOfCrates - 1
        
        match newCount with
        | 0 -> stackState
        | _ -> moveCrates stackState newCount fromStack toStack
    
    let folder stackState input =
        match InstructionMatcher().TryTypedMatch(input) with
        | Some m -> 
            let adjustedSourceStackIdx = m.source.AsInt - 1
            let adjustedTargetStackIdx = m.destination.AsInt - 1
            moveCrates stackState m.itemCount.AsInt adjustedSourceStackIdx adjustedTargetStackIdx
        |_ -> stackState
    
    Seq.fold folder initialState inputData 
    
let result = rearrangeCrates inputData |> Seq.choose Seq.tryHead |> String.concat ""
printfn "%A" result