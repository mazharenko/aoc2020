//
// https://adventofcode.com/2020/day/13
//

#if !INTERACTIVE
module Puzzle13
#else
#load "common.fs"
#endif

open System.IO
open common


module Part1 = 
    let (targetTs, buses) =
        let rows = Path.Combine(__SOURCE_DIRECTORY__, "puzzle13.txt")
                |> readLines
                |> Seq.toArray
        let times = rows.[1].Split(",") |> Seq.filter ((<>)"x") |> Seq.map int64
        int64 rows.[0]
        ,
        times |> Array.ofSeq
        

    let timeToWait ts busId =
        let x = (ts % busId)
        abs (x - busId)
        
    let answer =
        let (minBus, minTime) = buses |> Array.map (fun bus -> bus, timeToWait targetTs bus) |> Seq.minBy snd
        minBus * minTime
        
        
module Part2 =
    let buses =
        let rows = Path.Combine(__SOURCE_DIRECTORY__, "puzzle13.txt")
                |> readLines
                |> Seq.toArray
        let times = rows.[1].Split(",") |> Array.map (fun s -> match s with |"x" -> 1, -1L | _ -> 1, int64 s) |> List.ofArray
        
        let rec readTimes l count =
            match l with
            | (_,-1L)::tail -> readTimes tail (count + 1)
            | (_,head)::tail -> (count, head) :: readTimes tail 1
            | [] -> []
            
        let head::tail = readTimes times 1
        (0, snd head)::tail
        
    let findClosest offset busId =
        offset + (busId - offset % busId)
        
    let rec findClosestWithNewBus buses offset bus diff =
        if (buses = []) then offset + (bus - offset % bus)
        else
            let newOffset = offset + int64 diff
            if (newOffset % bus = 0L) then newOffset
            else
            findClosestWithNewBus buses (offset + (buses |> List.fold (*) 1L)) bus diff
        
    let rec findResult buses offset =
        let lastoffset =
            buses
            |> List.fold (fun (offset, busesAcc) (diff, bus) ->
                        let newOffset = findClosestWithNewBus busesAcc offset bus diff
                        newOffset, busesAcc @ [bus])
               (offset, [])
            |> fst
        lastoffset - (List.sumBy fst buses |> int64)
        
    let result = findResult buses 0L
    
    
     