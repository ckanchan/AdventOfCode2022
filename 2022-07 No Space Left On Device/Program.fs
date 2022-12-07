#if INTERACTIVE
#r "nuget:FSharp.Text.RegexProvider"
#endif

open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

let testData = File.ReadLines "./test"
let inputData = File.ReadLines "./input"

