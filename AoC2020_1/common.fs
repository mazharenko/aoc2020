module common

open System.IO

let readLines (filePath: string): seq<string> =
    File.ReadLines filePath
    |> Seq.filter (System.String.IsNullOrEmpty >> not)
