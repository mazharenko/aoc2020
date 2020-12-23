//
// https://adventofcode.com/2020/day/23
//

#if !INTERACTIVE
module Puzzle23
#else
#load "common.fs"
#endif

open common

let input = ("198753462" |> Seq.toArray |> Array.map (string >> int))

let prepareInput (input: int[]) =
    let prepared = Array.zeroCreate (input.Length + 1)
    input
    |> Array.iteri (fun i n -> prepared.[n] <- if (i = input.Length - 1) then input.[0] else input.[i + 1])
    prepared, input |> Array.max, input.[0]

   
let move current max (ar: int[])  =
    let next1 = ar.[current]
    let next2 = ar.[next1]
    let next3 = ar.[next2]
    let newCurrent = ar.[next3]
    let destinationCandidates =
        seq {
            yield! {current .. (-1).. 1}
            yield! {max..(-1)..1}
        }
        |> Seq.except (seq {current; next1; next2; next3})
    let destination = destinationCandidates |> Seq.head
    let destinationNext = ar.[destination]
    
    ar.[destination] <- next1
    ar.[next3] <- destinationNext
    ar.[current] <- newCurrent
    newCurrent
    
let rec toList current limit (ar:int[]) =
    if (limit <= 0) then []
    else
        current::(toList ar.[current] (limit - 1) ar)


let puzzle23_1 =
    let (ar, max, current) = prepareInput input
    let res = 
        [1..100]
        |> List.fold (fun cur i -> move cur max ar) current
    (res, ar) ||> (fun _ -> toList 1 (ar.Length - 1))
    |> List.skip 1

let puzzle23_2 =
    let (ar, max, current) = prepareInput (Array.append input [|10..1000000|])
    let res = 
        [1..10000000]
        |> List.fold (fun cur i -> move cur max ar) current
    (res, ar) ||> (fun _ -> toList 1 3)
    |> List.skip 1
    |> List.map int64
    |> List.reduce (*)