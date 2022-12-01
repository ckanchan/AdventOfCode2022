open System

let testData = IO.File.ReadAllText "./test"
let inputData = IO.File.ReadAllText "./input"

type Elf = 
    | Elf of meals: int array

let totalCalories (Elf meals) =
    Array.sum(meals)


let elvesFromInput (input: string) =
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map int)
    |> Array.map Elf
    
let mostCalories  = Array.map totalCalories >> Array.max

let mostCaloriesOfInput =
    inputData
    |> elvesFromInput
    |> mostCalories

printfn "%d" mostCaloriesOfInput

let topThree =
     Array.map totalCalories >> Array.sortDescending >> Array.take 3