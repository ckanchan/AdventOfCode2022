open System.IO

let testData = File.ReadAllLines "./test"
let inputData = File.ReadAllLines "./input"

type NeighbouringTrees = 
    { above: int array; below: int array; left: int array; right: int array }

let NeighbouringTreesToList (trees: NeighbouringTrees) =
    [trees.above; trees.below; trees.left; trees.right]

let treeMap data =
    data 
    |> Array.map (fun (str: string) -> str.ToCharArray() |> Array.map (fun char -> int (char.ToString())) )
    |> array2D

let dimensions treeMap =
    let rowCount = Array2D.length1 treeMap
    let columnCount = Array2D.length2 treeMap

    {| rows = rowCount; columns=columnCount |}



let neighbouringTrees (treeMap: int[,]) row column = 
    let dimensions = dimensions treeMap
    let topTrees = treeMap[0..row, column] |> Array.take row
    let bottomTrees = treeMap[row+1..dimensions.rows, column]
    let leftTrees = treeMap[row, 0..column] |> Array.take column  
    let rightTrees = treeMap[row,(column + 1)..dimensions.columns]
    {above = topTrees; below = bottomTrees; left = leftTrees; right = rightTrees}
        
let isVisible (treeMap: int[,]) row column =
    let dimensions = dimensions treeMap
    
    let maxRowIdx = dimensions.rows - 1
    let maxColIdx = dimensions.columns - 1

    match (row, column) with
        | (0, _) -> true
        | (_, 0) -> true
        | (row, _) when row = maxRowIdx-> true
        | (_, column) when column = maxColIdx -> true
        | (_,_) ->
                let heightOfTree = treeMap[row, column] 
                let isShorterThanInputTree neighbourTree =
                    neighbourTree < heightOfTree


                neighbouringTrees treeMap row column
                |> NeighbouringTreesToList
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

let scenicScore (treeMap: int[,]) row column =
    let height = treeMap[row,column]
    let dimensions = dimensions treeMap
    let neighbours = neighbouringTrees treeMap row column

    let calculateViewingDistance trees =
        let folder (acc: (bool * int)) elem =
            let (blocked, count) = acc
            match blocked with
            | true -> (true, count)
            | false ->
                if elem >=height 
                    then (true, count + 1)
                    else (false, count + 1)     
        
        if Array.isEmpty trees then 0 else
            let (_, result) = Array.fold folder (false, 0) trees
            result
    
    let leftViewingScore = neighbours.left |> Array.rev |> calculateViewingDistance
    let aboveViewingScore = neighbours.above |> Array.rev |> calculateViewingDistance
    let rightViewingScore = neighbours.right |> calculateViewingDistance
    let belowViewingScore = neighbours.below |> calculateViewingDistance
    
    List.reduce (*) [leftViewingScore; aboveViewingScore; rightViewingScore; belowViewingScore]

let maxScenicScore treeMap =
    let scenicScoreforMap = scenicScore treeMap

    treeMap
    |> Array2D.mapi (fun row col elem -> scenicScoreforMap row col)
    |> Seq.cast<int>
    |> Seq.max

let tree = treeMap inputData

countVisibleTrees tree |> printfn "%d"

maxScenicScore tree |> printfn "%d"