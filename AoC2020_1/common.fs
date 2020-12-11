module common

open System.IO

let readLines (filePath: string): seq<string> =
    File.ReadLines filePath
    |> Seq.filter (System.String.IsNullOrEmpty >> not)

let read2d (filePath: string) =
    let lines = readLines filePath |> Array.ofSeq
    let length = Seq.head lines |> String.length
    let height = Array.length lines

    Array2D.init height length (fun r c -> lines.[r].[c])

