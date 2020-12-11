//
// https://adventofcode.com/2020/day/5
//

#if !INTERACTIVE
module Puzzle5
#else
#load "common.fs"
#endif

open System.IO
open common


let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle5.txt")
    |> readLines
    |> Array.ofSeq

let rec searchRow (``begin``:int, ``end``:int) (s:string) pos =
    if (``begin`` = ``end``) then ``begin``
    else match s.[pos] with
            | 'F' -> searchRow (``begin``, (``begin`` + ``end``) / 2) s (pos + 1)
            | 'B' -> searchRow ((``begin`` + ``end``) / 2 + 1, ``end``) s (pos + 1)
            | _ -> invalidArg "s" "wrong symbol"

let rec searchCol (``begin``:int, ``end``:int) (s:string) pos =
    if (``begin`` = ``end``) then ``begin``
    else match s.[pos] with
            | 'L' -> searchCol (``begin``, (``begin`` + ``end``) / 2) s (pos + 1)
            | 'R' -> searchCol ((``begin`` + ``end``) / 2 + 1, ``end``) s (pos + 1)
            | _ -> invalidArg "s" "wrong symbol"

let seat (s: string) =
    (
        searchRow (0, 127) s 0,
        searchCol (0, 7) s 7
    )
    
let id (row, col) =
    row * 8 + col
    
let puzzle5_1 =
    input
    |> Seq.map (seat >> id)
    |> Seq.max
    
let puzzle5_2 =
    let allIds =  [0..127 * 8+7]
    let occupied =
        input
        |> Seq.map (seat >> id)
        |> Set.ofSeq
    allIds
    |> Seq.where (fun x -> Set.contains x occupied |> not && Set.contains (x + 1) occupied && Set.contains (x - 1) occupied)
    |> Seq.exactlyOne