//
// https://adventofcode.com/2020/day/24
//

#if !INTERACTIVE
module Puzzle24
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System
open System.IO
open FParsec
open common

let pPath =
    let p =
        attempt (pstring "e")
        <|> attempt (pstring "se")
        <|> attempt (pstring "sw")
        <|> attempt (pstring "w")
        <|> attempt (pstring "nw")
        <|> attempt (pstring "ne")
    many1 p
    
let input =
    readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle24.txt"))
    |> Seq.map (run pPath)
    |> Seq.map (function
        | Success(result, _, _) -> result
        | _ -> failwith "not parsed"
    ) |> List.ofSeq

let floor = Array2D.createBased -500 -500 1000 1000 0

let coorinates p path =
    
    let step (i, j) direction =
        match (direction) with
        | "e" -> i - 1, j
        | "se" -> i, j+1
        | "sw" -> i+1, j+1
        | "w" -> i+1, j
        | "nw" -> i, j-1
        | "ne" -> i-1, j-1        
        | _ -> failwith "unknown direction"
    
    path
    |> List.fold step p
    
    
let flip floor path =
    let p = coorinates (0, 0) path
    Array2D.mapi (fun i j color -> if (p = (i,j)) then Convert.ToInt32((color = 0)) else color) floor
    
let flipAll floor paths =
    paths |> List.fold flip floor 

let artDay (floor: int[,]) =
    let adjacent p =
        [
            ["e"] 
            ["se"]
            ["sw"]
            ["w"] 
            ["nw"]
            ["ne"]
        ] |> List.map (coorinates p)
    Array2D.mapi (fun i j color ->
            if (i < -490 || j < -490 || j >= 490 || i >= 490) then color
            else
                let adj = adjacent (i, j)
                let adjBlack = adj
                              |> List.map(fun (adji, adjj) ->
                                    floor.[adji, adjj])
                              |> List.filter ((=) 1)
                              |> List.length
                match color with
                | 1 -> if (adjBlack = 0 || adjBlack > 2) then 0 else 1
                | 0 -> if (adjBlack = 2) then 1 else 0
                | _ -> failwith ""
                
        ) floor


let puzzle24_1 =
    flipAll floor input
    |> Seq.cast<int> |> Seq.filter ((=)1)
    |> Seq.length
    
let puzzle24_2 =
    [1..100]
    |> List.fold (fun fl i -> artDay fl) (flipAll floor input)
    |> Seq.cast<int> |> Seq.filter ((=)1)
    |> Seq.length
    
