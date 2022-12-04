open System

let testData = IO.File.ReadLines "./test"
let inputData = IO.File.ReadLines "./input"

let strToCharArray (str: string) =
    str.ToCharArray()

let itemToPriority character = 
    let charList = {'A'..'Z'} |> Seq.append {'a'..'z'}
    {1..52}
    |> Seq.zip charList
    |> Map.ofSeq
    |> Map.find character

let getCommon (ruckSack: string) =
    ruckSack
    |> strToCharArray
    |> Array.splitInto 2
    |> Array.map Set
    |> Array.reduce Set.intersect

let prioritiesOfCommonItem =
    getCommon >> Set.map itemToPriority >> Set.toSeq >> Seq.sum

let getCommonInTripleGroup (ruckSack1:string) (ruckSack2: string)(ruckSack3: string) =
    [|ruckSack1; ruckSack2; ruckSack3|] 
        |> Array.map strToCharArray
        |> Array.map Set.ofArray
        |> Array.reduce Set.intersect

let prioritiesOfCommonGroup ruckSack1 ruckSack2 ruckSack3 =
    getCommonInTripleGroup ruckSack1 ruckSack2 ruckSack3 
    |> Set.map itemToPriority 
    |> Set.toSeq 
    |> Seq.sum

let sums (input: string seq) =
    input
    |> Seq.chunkBySize 3
    |> Seq.map (fun chunk -> prioritiesOfCommonGroup chunk[0] chunk[1] chunk[2])
    |> Seq.sum

