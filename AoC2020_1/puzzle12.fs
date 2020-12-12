//
// https://adventofcode.com/2020/day/12
//

#if !INTERACTIVE
module Puzzle12
#else
#load "common.fs"
#endif

open System.IO
open common


let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle12.txt")
    |> readLines
    |> Seq.toArray

module Part1 = 

    type State = {east: int; north: int; degree: int; route: string[]; i: int}
        with member this.nexti with get () = {this with i = this.i + 1}

    let rec navigate state =
        if (state.i >= state.route.Length)
        then state
        else
            let command = state.route.[state.i].[0]
            let arg = state.route.[state.i].Substring 1 |> int
            let rec execute command' arg' =
                match command' with
                | 'N' -> {state with north = state.north + arg'}.nexti
                | 'S' -> execute 'N' -arg'
                | 'E' -> {state with east = state.east + arg'}.nexti
                | 'W' -> execute 'E' -arg'
                | 'R' -> {state with degree = state.degree + arg'}.nexti
                | 'L' -> execute 'R' -arg'
                | 'F' -> match (state.degree/90 % 4) with
                         | 0 -> execute 'N' arg'
                         | 1 | -3 -> execute 'E' arg'
                         | 2 | -2 -> execute 'S' arg'
                         | 3 | -1 -> execute 'W' arg'
                         | _ -> failwithf "wrong degree %i" state.degree
                | _ -> failwithf "unknown command %c" command'
            execute command arg |> navigate
           
    
module Part2 =
    
    type State = {east: int; north: int; wpEast: int; wpNorth: int; route: string[]; i: int}
        with member this.nexti = {this with i = this.i + 1}
    
    let rec navigate state =
        if (state.i >= state.route.Length)
        then state
        else
            let command = state.route.[state.i].[0]
            let arg = state.route.[state.i].Substring 1 |> int
            let rec execute command' arg' =
                match command' with
                | 'N' -> {state with wpNorth = state.wpNorth + arg'; i = state.i + 1}
                | 'S' -> execute 'N' -arg'
                | 'E' -> {state with wpEast = state.wpEast + arg'; i = state.i + 1}
                | 'W' -> execute 'E' -arg'
                | 'R' -> match (arg'/90 % 4) with
                         | 0 -> {state with i = state.i + 1}
                         | 1 | -3 -> {state with wpEast = state.wpNorth; wpNorth = -state.wpEast}.nexti
                         | 2 | -2 -> {state with wpEast = -state.wpEast; wpNorth = -state.wpNorth}.nexti
                         | 3 | -1 -> {state with wpEast = -state.wpNorth; wpNorth = state.wpEast}.nexti
                         | _ -> failwithf "wrong degree %i" arg'
                | 'L' -> execute 'R' -arg'
                | 'F' -> {state with
                                east = state.east + arg' * state.wpEast
                                north = state.north + arg' * state.wpNorth
                                i = state.i + 1}
                | _ -> failwithf "unknown command %c" command'
            execute command arg |> navigate
        

let puzzle12_1 =
    let resultState = Part1.navigate {east = 0; north = 0; degree = 90; route = input; i = 0}
    abs resultState.north
    +
    abs resultState.east
    
let puzzle12_2 =
    let resultState = Part2.navigate {east = 0; north = 0; wpEast = 10; wpNorth = 1; route = input; i = 0}
    abs resultState.north
    +
    abs resultState.east