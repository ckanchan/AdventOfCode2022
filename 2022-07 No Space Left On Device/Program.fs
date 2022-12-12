open System.IO
open FileSystem
open Terminal

let testData = File.ReadLines "./test"
let inputData = File.ReadAllLines "./input"

let dirInfo = terminalOutputToDirectoryInfo  inputData
let maxDepth = maxDepth dirInfo
let tree = buildFileSystem maxDepth Map.empty dirInfo
let result = 
    directorySizes List.empty tree
    |> List.filter (fun item -> (snd item) < 100_000)
    |> List.map snd
    |> List.sum

let diskSize = 70000000
let usedSpace = totalSize tree
let remainingFreeSpace = diskSize - usedSpace

let updateSize = 30000000
let sizeRequired = updateSize - remainingFreeSpace

let directoryToDelete =
    directorySizes List.empty tree
    |> List.filter (fun item -> (snd item) >= sizeRequired)
    |> List.minBy (fun item -> snd item)
    
printfn "Delete %s of size %d" (fst directoryToDelete) (snd directoryToDelete)