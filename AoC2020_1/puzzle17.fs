//
// https://adventofcode.com/2020/day/17
//

#if !INTERACTIVE
module Puzzle17
#else
#load "common.fs"
#endif

open System.IO
open common

let input = read2d (Path.Combine(__SOURCE_DIRECTORY__, "puzzle17.txt"))

type State = | Active | Inactive
        
type Cube = (int * int * int * int)

let dimension =
    input
    |> Array2D.mapi (fun i j c -> (i, j, 0, 0), match c with | '#' -> Active | _ -> Inactive)
    |> Seq.cast<Cube * State>
    |> Map.ofSeq
    
let evolve adjacent dimension  =
    let activeKeys = Map.toSeq dimension |> Seq.filter (snd >> (=)Active) |> Seq.map fst |> Array.ofSeq        
    let activeAdjacent = activeKeys |> Seq.collect adjacent |> Array.ofSeq 
    
    let newDimension = 
        activeAdjacent
        |> Seq.groupBy id |> Seq.map (fun (p, s) -> p, Seq.length s)
        |> Seq.map (fun (cube, activeAdjCount) ->
                        let state = dimension |> Map.tryFind cube
                        match state, activeAdjCount with
                        | Some Active, 2 -> cube, Active
                        | _, 3 -> cube, Active
                        | _, _ -> cube, Inactive)
        |> Map.ofSeq
        
    newDimension

module Part1 = 
    
    let adjacent (i, j, k, _) =
        [k-1..k+1]
        |> Seq.allPairs [j-1..j+1]
        |> Seq.allPairs [i-1..i+1] 
        |> Seq.map (fun (i', (j', k')) -> i', j', k', 0)
        |> Seq.except [i,j,k,0]
        
    let answer = 
        dimension 
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> Map.toSeq
        |> Seq.filter (snd >> (=)Active) |> Seq.length
        
module Part2 =
    
    let adjacent (i, j, k, z) =
        [z-1..z+1]
        |> Seq.allPairs [k-1..k+1]
        |> Seq.allPairs [j-1..j+1]
        |> Seq.allPairs [i-1..i+1] 
        |> Seq.map (fun (i', (j', (k', z'))) -> i', j', k', z')
        |> Seq.except [i,j,k,z]
        
    let answer = 
        dimension
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> evolve adjacent
        |> Map.toSeq
        |> Seq.filter (snd >> (=)Active) |> Seq.length