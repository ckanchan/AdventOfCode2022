open System.IO
open System.Diagnostics

let testData = File.ReadLines "./test"
let inputData = File.ReadLines "./input"

let parseInt (str: string) =
    match System.Int32.TryParse(str) with 
    | (true, num) -> Some(num)
    | _ -> None
        
[<Struct>][<DebuggerDisplay("{print()}")>]
type PointXY = 
    {x: int; y: int}
    member point.print = sprintf "(%d,%d)" point.x point.y
    static member origin = {x=0;y=0}

[<Struct>][<DebuggerDisplay("{print()t}")>]
type Rope = 
    {head: PointXY; tail: PointXY}
    member rope.print = sprintf "||head: %s | tail: %s||" rope.head.print rope.tail.print

let startPosition = {head=PointXY.origin; tail=PointXY.origin}

type Direction = Left | Right | Up | Down
let directionFromString str = 
    match str with
    | "U" -> Some Up
    | "D" -> Some Down
    | "L" -> Some Left
    | "R" -> Some Right
    | _ -> None

type Move = {direction: Direction; magnitude: int}

let parseLine (line: string) =
    let elements = line.Split(" ")
    let direction = directionFromString elements[0]
    let distance = parseInt elements[1]

    match (direction, distance) with
        | Some direction, Some distance -> Some  {direction = direction; magnitude = distance}
        | _ -> None

let transform direction distance point =
    match direction with
    | Left -> { point with x = point.x - distance }
    | Right -> { point with x = point.x + distance }
    | Up -> { point with y = point.y + distance }
    | Down -> { point with y = point.y - distance }

type DiagonalDirection = UpperLeft | UpperRight | LowerLeft | LowerRight
type Neighbour = 
    | Overlap 
    | Above of magnitude:int
    | Below of magnitude: int
    | Left of magnitude: int
    | Right of magnitude: int
    | Diagonal of direction: DiagonalDirection * horizontalMagnitude: int * verticalMagnitude: int


type Comparison = GreaterThan | LessThan | Equal
let compare num1 num2 = 
    if num1 = num2 then Equal
    else if num1 > num2 then GreaterThan
    else LessThan



let positionDifference point1 point2 =
    let verticalDifference = compare point1.y point2.y
    let horizontalDifference = compare point1.x point2.x
    let verticalMagnitude = abs (point1.y - point2.y)
    let horizontalMagnitude = abs (point1.x - point2.x)
    
    match (horizontalDifference, verticalDifference) with
        | (Equal, Equal) -> Overlap
        | (GreaterThan, Equal) -> Right horizontalMagnitude
        | (LessThan, Equal) -> Left horizontalMagnitude
        | (Equal, GreaterThan) -> Above verticalMagnitude
        | (Equal, LessThan) -> Below verticalMagnitude
        | (GreaterThan, LessThan) -> Diagonal (LowerRight, horizontalMagnitude, verticalMagnitude)
        | (GreaterThan, GreaterThan) -> Diagonal (UpperRight, horizontalMagnitude, verticalMagnitude)
        | (LessThan, LessThan) -> Diagonal (LowerLeft, horizontalMagnitude, verticalMagnitude)
        | (LessThan, GreaterThan) -> Diagonal (UpperLeft, horizontalMagnitude, verticalMagnitude)
    



let moveRope (by: Move) (rope: Rope) =
    let newTailPosition tailPosition headPosition =
        match positionDifference headPosition tailPosition with
        | Overlap -> tailPosition
        | Left magnitude when magnitude = 1 -> tailPosition
        | Right magnitude when magnitude = 1 -> tailPosition
        | Above magnitude when magnitude = 1 -> tailPosition
        | Below magnitude when magnitude = 1 -> tailPosition
        | Diagonal (_, horizontalMagnitude, verticalMagnitude) when (horizontalMagnitude, verticalMagnitude) = (1,1) -> tailPosition
        | Left _ -> {tailPosition with x = tailPosition.x - 1}
        | Right _ -> {tailPosition with x = tailPosition.x + 1}
        | Above _ -> {tailPosition with y = tailPosition.y + 1}
        | Below _ -> {tailPosition with y = tailPosition.y - 1}
        | Diagonal (diagonalDirection, _, _) ->
            match diagonalDirection with
            | UpperLeft -> {tailPosition with x = tailPosition.x - 1; y = tailPosition.y + 1}
            | UpperRight -> {tailPosition with x = tailPosition.x + 1; y = tailPosition.y + 1}
            | LowerLeft -> {tailPosition with x = tailPosition.x - 1; y = tailPosition.y - 1}
            | LowerRight -> {tailPosition with x = tailPosition.x + 1; y = tailPosition.y - 1}      

    let newRopePositions rope =
        [|1..by.magnitude|]
        |> Array.scan (
            fun acc elem ->
                let newHead = transform by.direction 1 acc.head
                let newTail = newTailPosition acc.tail newHead
                {head=newHead; tail=newTail}
        ) rope
            
    newRopePositions rope

    
let ropesFromStringSeq (s: string seq) =
    s
    |> Seq.choose parseLine
    |> Seq.fold (fun acc elem ->
            let ropeEndPosition = Array.last acc
            let newPositions = moveRope elem ropeEndPosition
            Array.append acc newPositions
    ) [|startPosition|]

let test =  ropesFromStringSeq testData
let input = ropesFromStringSeq inputData


let printRopes (ropeArray: Rope array) =
    ropeArray 
    |> Array.map (fun r -> r.print)
    |> Array.iter (printfn "%s")

let countDistinctTailPositions ropes = 
    ropes |> Array.map (fun r -> r.tail) |> Array.distinct |> Array.length


