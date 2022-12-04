#if INTERACTIVE
#r "nuget: FSharp.Text.RegexProvider"
#endif

open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

let input = File.ReadLines("./input")
let test =  File.ReadLines("./test")

type AssignmentPairRegex = Regex< @"(?'pair1lower'\d+)-(?'pair1upper'\d+),(?'pair2lower'\d+)-(?'pair2upper'\d+)" >

let re = AssignmentPairRegex()

let stringPairsToSets (str: string) =
    let matched = re.TypedMatch(str)
    let set1 = [matched.pair1lower.AsInt..matched.pair1upper.AsInt] |> Set.ofList
    let set2 = [matched.pair2lower.AsInt..matched.pair2upper.AsInt] |> Set.ofList

    (set1, set2)

let assignmentPairsFullContainment (set1, set2) =
    (set1 |> Set.isSubset set2) || (set2 |> Set.isSubset set1)

let countTrue seq = seq |> Seq.fold (fun (countTrue: int)(next: bool) -> if next then countTrue + 1 else countTrue) 0

let processInput withFunc =
    Seq.map stringPairsToSets >> Seq.map withFunc >> countTrue

let pairsContainingOther: string seq -> int =
    Seq.map stringPairsToSets >> Seq.map assignmentPairsFullContainment >> countTrue

let assignmentPairsIntersect (set1, set2) =
    set1 |> Set.intersect set2 |> Set.isEmpty |> not

let pairsIntersecting: string seq -> int =
    processInput assignmentPairsIntersect


