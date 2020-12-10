#if !INTERACTIVE
module Puzzle10
#else
#load "common.fs"
#endif

open System
open System.IO
open common

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle10.txt")
    |> readLines
    |> Seq.map int
    |> Seq.sort
    |> Seq.toArray
    
    
let max =
    input.[input.Length - 1]
    
let diffs adapters =
    let allSeq = seq {
        yield 0
        yield! adapters
        yield max + 3
    }
    allSeq 
    |> Seq.pairwise
    |> Seq.map (fun (x, y) -> y - x)
    
let countPaths adapters =
    let allSeq =
        seq {
            yield -10
            yield -10 // extra for correct windows
            yield 0
            yield! adapters
            yield max + 3
        }
    allSeq
    |> Seq.windowed 4
    |> Seq.fold (
        // (-10 -> -10 -> 0) -> 1 -> 4 -> 5 -> 6 -> 7 -> 10 -> 11 -> 12 -> 15 -> 16 -> 19
        //          1     1     1    1    1    2    4     4    4     8     8      8     8
        //                          \_______________?    \___________?    \_______?
        fun paths window ->
            let current = window.[3]
            let pathsToCurrent =
                window
                |> Array.map (fun x -> current - x)
                |> Seq.zip paths
                |> Seq.where (fun (_, distance) -> distance <= 3)
                |> Seq.map fst
                |> Seq.sum
                
            [|paths.[1]; paths.[2]; pathsToCurrent|]
    ) [|1L; 1L; 1L|]
    |> Array.last
    
    
let puzzle10_1 =
    let counts =
        diffs input
        |> Seq.countBy id
        |> Map.ofSeq
    counts.[1] * counts.[3]

let puzzle10_2 =
    countPaths input