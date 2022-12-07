#if INTERACTIVE
#r "nuget:FSharp.Text.RegexProvider"
#endif

open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

let testData = File.ReadLines "./test"
let inputData = File.ReadLines "./input"

type FileSystem =
    | File of name: string * size: int
    | Directory of name: string * contents: FileSystem list

type FileMatcher = Regex< @"(?'size'\d+) (?'name'\w+)(?'ext'.\w+)">