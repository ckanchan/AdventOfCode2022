open System.IO

let testData = File.ReadAllLines "./test"
let inputData = File.ReadAllLines "./input"

let treeMap data =
    data 
    |> Array.map (fun (str: string) -> str.ToCharArray() |> Array.map (fun char -> int (char.ToString())) )
    |> array2D

let isVisible (treeMap: int[,]) row column =
    let rowCount = Array2D.length1 treeMap
    let columnCount = Array2D.length2 treeMap

    let maxRowIdx = rowCount - 1
    let maxColIdx = columnCount - 1

    let neighbouringTrees row column = 
        let topTrees = treeMap[0..row, column] |> Array.take row
        let bottomTrees = treeMap[row+1..rowCount, column]
        let leftTrees = treeMap[row, 0..column] |> Array.take column  
        let rightTrees = treeMap[row,(column + 1)..columnCount]
        [topTrees; bottomTrees; leftTrees; rightTrees]

    match (row, column) with
        | (0, _) -> true
        | (_, 0) -> true
        | (row, _) when row = maxRowIdx-> true
        | (_, column) when column = maxColIdx -> true
        | (_,_) ->
                let heightOfTree = treeMap[row, column] 
                let isShorterThanInputTree neighbourTree =
                    neighbourTree < heightOfTree


                neighbouringTrees row column
                |> List.exists (
                    fun trees -> 
                    trees  |> Array.forall isShorterThanInputTree 
                    )
                    
let countVisibleTrees (treeMap: int[,]) =
    let isVisibleInTreeMap = isVisible treeMap
    
    treeMap
    |> Array2D.mapi (fun row col elem -> isVisibleInTreeMap row col)
    |> Seq.cast<bool>
    |> Seq.filter (fun x -> x = true)
    |> Seq.length