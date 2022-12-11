module Terminal

open FileSystem
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type FileMatcher = Regex< @"(?'size'\d+) (?'name'\w+)(?((?'ext'.\w+)))">
type DirectoryMatcher = Regex< @"dir (?'dirName'\w+)">
type UserCommandMatcher = Regex< @"\$ (?'command'\w+)(?( (?'args'\w+|\/)))">

type DirectoryChange = 
    | ParentDirectory
    | RootDirectory
    | NamedDirectory of name: string

type UserCommand =
    | ChangeDirectory of DirectoryChange
    | ListCurrentDirectory

type TerminalOutput = Command of UserCommand | FS of FileSystem


type DirectoryInfo = {name: string; depth: int; parent: string option; children: File list}
type DirectoryBuildState = (Map<string, DirectoryInfo> * DirectoryInfo)

let parseUserCommand str = 
    match UserCommandMatcher().TryTypedMatch str with 
    | Some m -> 
        let command = m.command.Value
        let arg = m.args.TryValue
        match (command, arg) with
        | ("ls", _ ) -> Some ListCurrentDirectory
        | ("cd", Some "/") -> ChangeDirectory RootDirectory |> Some
        | ("cd", Some "..") -> ChangeDirectory ParentDirectory |> Some
        | ("cd", Some dir) -> ChangeDirectory (NamedDirectory dir) |> Some
        | (_, _) -> None
    | None -> None

let parseSystemOutput str =
    match DirectoryMatcher().TryTypedMatch str with
        | Some m -> Directory {name=m.dirName.Value; contents=List.Empty} |> Some
        | None ->
            match FileMatcher().TryTypedMatch str with
                | Some m 
                    ->  match m.ext.TryValue with
                        | Some ext ->  File {name=m.name.Value + ext;size= m.size.AsInt}|> Some
                        | None -> File {name=m.name.Value; size=m.size.AsInt}|> Some
                | None -> None

let parseLineOfInput inp: TerminalOutput option = 
    match parseUserCommand inp with 
        | Some command -> Some (Command command)
        | None -> 
            match parseSystemOutput inp with 
                | Some sysOutput -> Some (FS sysOutput)
                | None -> None

let terminalOutputToDirectoryInfo output = 
    let emptyBuildState: DirectoryBuildState = (Map.empty, {name="";depth=0;parent=None;children=[]})
    let directoryInfoFolder (directoryBuild: DirectoryBuildState) newLine =
        let (infoMap, currentDir) = directoryBuild
        match newLine with 
        | FS f ->
            match f with 
                | Directory d -> 
                    let newDirectory = {name=d.name; depth = currentDir.depth + 1; parent=Some currentDir.name; children=[]}
                    let newMap = Map.add d.name newDirectory infoMap
                    (newMap, currentDir)
                | File f ->
                    let updateDirectory = {currentDir with children=f::currentDir.children}
                    let newMap = Map.add updateDirectory.name updateDirectory infoMap
                    (newMap, updateDirectory)
        | Command cmd ->
            match cmd with
                | ListCurrentDirectory -> directoryBuild
                | ChangeDirectory chDir ->
                    match chDir with 
                        | RootDirectory -> 
                            match Map.tryFind "/" infoMap with 
                                | Some root -> (infoMap, root)
                                | None -> 
                                    let root = {name="/"; depth=0; parent=None; children=[]}
                                    let newMap = Map.add "/"  root Map.empty
                                    (newMap, root)
                        | NamedDirectory n ->
                            let target = Map.find n infoMap
                            (infoMap, target)
                        | ParentDirectory ->
                            match currentDir.parent with 
                                | Some parent ->
                                    let target = Map.find parent infoMap
                                    (infoMap, target)
                                | None ->
                                    let root = Map.find "/" infoMap
                                    (infoMap, root)
    
    output 
    |> Seq.choose parseLineOfInput 
    |> Seq.fold directoryInfoFolder emptyBuildState 
    |> fst

let maxDepth (mappedDirectoryInfo: Map<string,DirectoryInfo>) =
    mappedDirectoryInfo |> Map.values |> Seq.map (fun info -> info.depth) |> Seq.max

let rec buildFileSystem atDepth (currentFSState: Map<string, FileSystem list>) (mappedInput: Map<string, DirectoryInfo>) =
    match atDepth with
        | 0 -> 
            let root = Map.find "/" mappedInput
            let childDirs = currentFSState |> Map.values |> Seq.reduce List.append
            Directory {
                name= "/"
                contents= (List.map File root.children) @ childDirs
            }
        | _ -> 
            let directoriesAtDepth = 
                mappedInput
                |> Map.values
                |> Seq.where (fun dirInfo -> dirInfo.depth = atDepth)
                |> Seq.map (
                    fun dirInfo ->
                        let childDirectories = currentFSState |> Map.tryFind dirInfo.name |> Option.defaultValue List.empty
                            
                        let newDirectory = Directory {
                            name=dirInfo.name
                            contents= (List.map File dirInfo.children) @ childDirectories
                        }   
                        let parent = Option.defaultValue "/" dirInfo.parent
                        (parent, newDirectory)               
                    )
                |> Seq.groupBy (fun pair -> fst pair)
                |> Seq.map (
                    fun triple ->
                        let (parentDir, directories) = triple
                        let justDirectories = directories |> Seq.map snd |> Seq.toList
                        (parentDir, justDirectories)
                    )
                |> Map.ofSeq
            
            let decrementDepth = atDepth - 1
            buildFileSystem decrementDepth directoriesAtDepth mappedInput