open System.IO
open FileSystem
open Terminal

let testData = File.ReadLines "./test"
let inputData = File.ReadLines "./input"

let dirDepthSorted mappedInput = 
    Map.values mappedInput
    |> Array.ofSeq
    |> Array.groupBy (fun dirInfo -> dirInfo.depth)
    |> Map.ofArray

// let (maxDepth, leafDirs) = Map.maxKeyValue dirDepthSorted

// let leafFSDirs =
//         leafDirs
//         |> Array.map (fun dirInfo ->
//             Directory {
//                 name=dirInfo.name
//                 contents= List.map File dirInfo.children
//             }
//         )



let dirInfo = terminalOutputToDirectoryInfo  testData
let maxDepth = maxDepth dirInfo
let tree = buildFileSystem maxDepth Map.empty dirInfo
directorySizes List.empty tree