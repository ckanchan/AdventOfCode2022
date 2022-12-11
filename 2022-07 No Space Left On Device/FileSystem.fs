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
            | File f -> List.empty 
            | Directory d ->
                let subDirectorySizes = d.contents |> List.collect (directorySizes currentDirList)
                let dirSize = totalSize (Directory d)
                [(d.name, dirSize)] @ subDirectorySizes   @ currentDirList

let countItems fs =
    let fileCount f = 1
    let dirCount (dirName, subcounts) = (List.sum subcounts) + 1

    cFileSystem fileCount dirCount fs
    