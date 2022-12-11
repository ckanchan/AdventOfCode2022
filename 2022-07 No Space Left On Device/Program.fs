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

printf "%d" result