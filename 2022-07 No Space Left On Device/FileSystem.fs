module FileSystem

type FileSystem =
    | File of File
    | Directory of Directory
and File = {name: string; size: int}
and Directory = {name: string; contents: FileSystem list}


// Taken from F Sharp for Fun and Profit
let rec cFileSystem fFile fDirectory (fs: FileSystem): 'r =
    match fs with
        | File f-> fFile f
        | Directory d-> 
            let subItems = d.contents |> List.map (cFileSystem fFile fDirectory)
            fDirectory (d.name, subItems)


let totalSize fileSystemItem =
    let fileSize f = f.size
    let dirSize (dirName, subSizes) = List.sum subSizes
    cFileSystem fileSize dirSize fileSystemItem

let rec directorySizes (currentDirList: (string * int) list) (currentItem: FileSystem) =
        match currentItem with 
            | File f -> [("file", f.size)]
            | Directory d ->
                let sizesOfChildren = d.contents |> List.collect (directorySizes currentDirList)
                let dirSize = 
                    sizesOfChildren 
                   //  |> List.where (fun pair -> (fst pair) = "file") 
                    |> List.map snd 
                    |> List.sum
                let subDirectorySizes = sizesOfChildren |> List.where (fun pair -> (fst pair) <> "file")
                [(d.name, dirSize)] @ subDirectorySizes   @ currentDirList

