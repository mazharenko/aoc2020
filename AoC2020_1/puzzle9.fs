//
// https://adventofcode.com/2020/day/9
//

#if !INTERACTIVE
module Puzzle9
#else
#load "common.fs"
#endif

open System.IO
open common

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle9.txt")
    |> readLines
    |> Seq.map int64
    |> Array.ofSeq
    

let window = 25

type NotPairSumSearchState = {window: Set<int64>; iBegin: int; iEnd: int; array: int64[]}
    with static member start ar wSize =
            {window = Seq.take wSize ar |> Set.ofSeq; iBegin = 0; iEnd = wSize; array = ar}

let rec findNorPairSum state =
    let {window = w; iBegin = begin'; iEnd = end'; array = ar} = state
    let target = ar.[end']
    let diffs = w |> Seq.map ((-)target) // if there are a, b: a+b=target, then we can search for b=target-a 
    let foundPair = diffs
                    |> Seq.filter (fun diff -> Set.contains diff w)
                    |> Seq.tryHead
    match foundPair with
    | None -> state
    | Some _ -> {
                    window =
                        w
                        |> Set.remove ar.[begin']
                        |> Set.add target
                    iBegin = begin' + 1
                    iEnd = end' + 1
                    array = ar
                } |> findNorPairSum
    
let puzzle9_1 =
    let res = NotPairSumSearchState.start input window
              |> findNorPairSum
    res.array.[res.iEnd]



type SumSearchState = {iBegin: int; iEnd: int; sum: int64; target: int64; array: int64[]}
    with static member start target (ar: int64[]) = {iBegin = 0; iEnd = 0; sum = ar.[0]; target = target; array = ar}

let rec findSum state =
    match Operators.compare state.sum state.target |> sign with
    | 1 -> findSum {
                state with iBegin = state.iBegin + 1
                           sum = state.sum - state.array.[state.iBegin]
            }
    | -1 -> findSum {
                state with iEnd = state.iEnd + 1
                           sum = state.sum + state.array.[state.iEnd + 1]
            }
    | _ -> state
    
let puzzle9_2 =
    let searchRes = findSum (SumSearchState.start puzzle9_1 input)
    let slice = input.[searchRes.iBegin..searchRes.iEnd]
    Array.max slice
    +
    Array.min slice