//
// https://adventofcode.com/2020/day/6
//


#if !INTERACTIVE
module Puzzle6

#else
#load "common.fs"
#endif

open System
open System.IO

let input =
    let text = File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, "puzzle6.txt"))
    text.Split ("\n\n", StringSplitOptions.RemoveEmptyEntries)
    
let puzzle6 countGroup =
    input
    |> Seq.map (fun x -> x.Split ("\n", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map countGroup
    |> Seq.sum
    
let puzzle6_1 = 
    let countGroup gLines =
        gLines
        |> Seq.collect id
        |> Set.ofSeq
        |> Seq.length
    puzzle6 countGroup
let puzzle6_2 = 
    let countGroup gLines =
        gLines
        |> Seq.map Set.ofSeq
        |> Set.intersectMany
        |> Seq.length
    puzzle6 countGroup