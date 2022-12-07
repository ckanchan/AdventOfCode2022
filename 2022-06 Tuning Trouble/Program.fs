open System.IO

let testData = File.ReadAllLines "./test"
let inputData = File.ReadAllLines "./input"

let startOfMarkerIdx (window: int) (s: string) =
    s.ToCharArray()
    |> Seq.windowed window
    |> Seq.takeWhile (fun charWindow -> Array.distinct charWindow |> Array.length <> window)
    |> Seq.length
    |> (+) window

let startOfPacketMarkerIdx = startOfMarkerIdx 4
let startOfMessageMarkerIdx = startOfMarkerIdx 14

inputData
|> Array.map startOfPacketMarkerIdx
|> Array.iter (printfn "%d")

inputData
|> Array.map startOfMessageMarkerIdx
|> Array.iter(printfn "%d")