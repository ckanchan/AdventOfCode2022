#if INTERACTIVE
    #r "nuget: FSharp.Text.RegexProvider";;
#endif

open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

module Monkey =
    [<Struct>] type Item = Item of uint64 with member this.Value = this |> fun (Item i) -> i
    [<Struct>] type NumberedMonkey = NumberedMonkey of int with member this.Value = this |> fun (NumberedMonkey m) -> m

    type HeldItem = {item: Item; heldBy: NumberedMonkey}

    type Operation =
        | Add of int
        | Multiply of int
        | Square
    with member this.Print =
            match this with
            | Add x -> sprintf "increases by %d" x
            | Multiply x -> sprintf "multiplied by %d" x
            | Square -> sprintf "multipled by itself"

    let invokeOperation operation (heldItem: HeldItem) =
        let newWorryLevel =
            match operation with
                | Add i -> heldItem.item.Value + (uint64 i)
                | Multiply m -> heldItem.item.Value * (uint64 m)
                | Square -> pown heldItem.item.Value 2
        // printfn "Monkey %d inspects an item with a worry level of %O" heldItem.heldBy.Value heldItem.item.Value
        // printfn "Worry level %s to %O" operation.Print newWorryLevel

        {heldItem with item = Item newWorryLevel}

    [<Struct>] type TestItem = Test of uint64 with member this.Value = this |> fun (Test t) -> t

    let invokeTest (test: TestItem) (trueMonkey: NumberedMonkey) (falseMonkey: NumberedMonkey) (heldItem: HeldItem) =
        if ((heldItem.item.Value % (uint64 test.Value)) = uint64 0)
            then
                // printfn "Current worry level is divisible by %d" test.Value
                {heldItem with heldBy=trueMonkey}
            else 
                // printfn "Current worry level is not divisible by %d" test.Value
                {heldItem with heldBy=falseMonkey}

module MonkeyParser =
    type MonkeyRegex = Regex< @"Monkey (?<MonkeyNumber>\d+):\n  Starting items: (?<Items>.+)\n  Operation: new = old (?<Operation>[+|*]) (?<OperationInt>(\d+|(old)))\n  Test: divisible by (?<TestInt>\d+)\n    If true: throw to monkey (?<TrueTarget>\d+)\n    If false: throw to monkey (?<FalseTarget>\d+)">

    type MonkeyInfo = {
        MonkeyNumber: Monkey.NumberedMonkey
        InitialItems: Monkey.HeldItem array
        Operation: Monkey.Operation
        Test: Monkey.TestItem
        TrueMonkey: Monkey.NumberedMonkey
        FalseMonkey: Monkey.NumberedMonkey
    }

    type ParsedMonkeyData = {
        heldItems: Map<Monkey.NumberedMonkey, Monkey.HeldItem array>
        monkeyOperations: (Monkey.HeldItem -> Monkey.HeldItem) array
        monkeyTests: (Monkey.HeldItem -> Monkey.HeldItem) array
        monkeyModulus: uint64
    }

    let monkeyParser = MonkeyRegex(System.Text.RegularExpressions.RegexOptions.Multiline)
    let parseMonkey (str: string) =
        let matched = monkeyParser.TypedMatch(str)
        let monkeyNumber = matched.MonkeyNumber.AsInt |> Monkey.NumberedMonkey
        let items = 
            matched.Items.Value.Split(",") 
            |> Array.map (fun n -> System.UInt64.Parse(n)  |> Monkey.Item )
            |> Array.map (fun i -> 
                {
                    Monkey.HeldItem.item=i
                    Monkey.HeldItem.heldBy=monkeyNumber
                }
            )            
        let operationInt = matched.OperationInt.TryAsInt
        let operation = 
            match operationInt with
            | Some operationInt ->
                match matched.Operation.Value with
                | "*" -> Monkey.Operation.Multiply operationInt
                | "+" -> Monkey.Operation.Add operationInt
            | None ->
                Monkey.Operation.Square
        let test = matched.TestInt.AsUInt64 |> Monkey.TestItem.Test
        let trueMonkey = matched.TrueTarget.AsInt |> Monkey.NumberedMonkey
        let falseMonkey = matched.FalseTarget.AsInt |> Monkey.NumberedMonkey

        {
            MonkeyNumber = monkeyNumber
            InitialItems = items
            Operation = operation
            Test = test
            TrueMonkey = trueMonkey
            FalseMonkey = falseMonkey
        }

    let sortParsedMonkeys = Array.sortBy (fun m -> m.MonkeyNumber.Value)
    let heldItems (monkeyInfo: MonkeyInfo array) = 
        monkeyInfo 
        |> Array.map (fun info -> (info.MonkeyNumber, info.InitialItems))
        |> Map
    
    let monkeyOperations (monkeyInfo: MonkeyInfo array) =
        monkeyInfo
        |> sortParsedMonkeys
        |> Array.map (fun m -> m.Operation |> Monkey.invokeOperation)
    
    let monkeyTests (monkeyInfo: MonkeyInfo array) =
        monkeyInfo
        |> sortParsedMonkeys
        |> Array.map (fun m -> Monkey.invokeTest m.Test m.TrueMonkey m.FalseMonkey)
    
    let monkeyModulus (monkeyInfo: MonkeyInfo array) =
        monkeyInfo |> Array.map (fun m -> m.Test.Value) |> Array.reduce (*)

    let monkeyDataFromStrings (strs: string array) =
        let parsedMonkeys = strs |> Array.map parseMonkey
        {
            heldItems = heldItems parsedMonkeys
            monkeyOperations = monkeyOperations parsedMonkeys
            monkeyTests = monkeyTests parsedMonkeys
            monkeyModulus = monkeyModulus parsedMonkeys
        }

let testData = File.ReadAllText("./test")
let inputData = File.ReadAllText("./input")

let parseData (str: string) =
    let monkeys = str.Split("\n\n")
    monkeys |> MonkeyParser.monkeyDataFromStrings


type MonkeyInspections = {items: Monkey.HeldItem array; inspections: uint64}
let InspectionItems inspection = inspection.items
let InspectionCounts inspection = inspection.inspections

let playRound 
    (operations: (Monkey.HeldItem -> Monkey.HeldItem) array)
    (tests:(Monkey.HeldItem -> Monkey.HeldItem) array)
    (modulus: uint64)
    (heldItems: Map<Monkey.NumberedMonkey,MonkeyInspections>) =
        let lowerWorryLevel (heldItem: Monkey.HeldItem) = 
            let newWorryLevel = heldItem.item.Value /  3UL
            printfn "Monkey gets bored with item. Worry level is divided by 3 to %O" newWorryLevel
            {heldItem with item = Monkey.Item (newWorryLevel)}

        let modulusWorryLevel (heldItem: Monkey.HeldItem) =
            let newWorryLevel = heldItem.item.Value % modulus
            {heldItem with item = Monkey.Item (newWorryLevel)}
        
        let monkeyActions =
            tests
            |> Array.map2 (fun op test -> op  >> modulusWorryLevel >> test) operations
        
        let accumulator (acc: Map<Monkey.NumberedMonkey, MonkeyInspections>) (elem: int) =
                let inspections = Map.find (Monkey.NumberedMonkey elem) acc
                let actions = Array.get monkeyActions elem
                let updatedItems = inspections.items |> Array.map actions
                let newByMonkey = updatedItems |> Array.groupBy (fun item -> item.heldBy) |> Map
                let newHeldItems = 
                    acc |> Map.map (
                    fun monkeyNum monkeyItems ->
                        let newItems = newByMonkey |> Map.tryFind monkeyNum
                        let inspectionBeforeTurn = acc |> Map.find monkeyNum
                        let itemsBeforeTurn = inspectionBeforeTurn.items
                        let itemsAfterTurn = 
                            match newItems with
                                | Some items -> items |> Array.append itemsBeforeTurn
                                | None -> itemsBeforeTurn
                        
                        if (elem = monkeyNum.Value) 
                            then {items = Array.empty; inspections = inspectionBeforeTurn.inspections + uint64 (Array.length inspections.items)}
                            else {inspectionBeforeTurn with items = itemsAfterTurn}
                )
                newHeldItems
        
        [|0..(heldItems.Count - 1)|]
        |> Array.fold accumulator heldItems

let printMonkeyMap (heldItems: Map<Monkey.NumberedMonkey, MonkeyInspections>) =
    heldItems 
    |> Map.iter (fun monkeyNum inspections ->
        let itemValues = inspections |> InspectionItems |> Array.map (fun i -> i.item.Value)
        printfn "Monkey %d: %A, inspected %d times" monkeyNum.Value itemValues inspections.inspections
    )

let totalMonkeyBusiness (monkeyInspections: Map<Monkey.NumberedMonkey, MonkeyInspections>) =
    monkeyInspections |> Map.values |> Seq.map InspectionCounts |> Seq.sort |> Seq.rev |> Seq.take 2 |> Seq.reduce (*)

let t = inputData |> parseData 
let initialState = 
    t.heldItems
    |> Map.map (fun k v -> {items=v; inspections = 0UL} )
let playRoundWithMonkeys = playRound t.monkeyOperations t.monkeyTests t.monkeyModulus

let result = [0..9999] |> List.fold (fun acc elem -> playRoundWithMonkeys acc) initialState

printMonkeyMap result
totalMonkeyBusiness result |> printfn "Total monkey business %d" 