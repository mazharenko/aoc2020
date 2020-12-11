//
// https://adventofcode.com/2020/day/11
//

#if !INTERACTIVE
module Puzzle11
#else
#load "common.fs"
#endif

open System.IO
open common

type Cell =
| Floor
| SeatEmpty
| SeatOccupied

type WaitingArea = WaitingArea of Cell[,]

let unwrap =
    function
    | WaitingArea cells -> cells
    
let create chars =
    chars
    |> Array2D.map (fun c -> match c with | '#' -> SeatOccupied | 'L' -> SeatEmpty | _ -> Floor)
    |> WaitingArea
    
let copy (wa: WaitingArea) =
    wa |> unwrap |> Array2D.copy |> WaitingArea
    

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle11.txt")
    |> read2d
    
module Array2D =
    let tryGet (ar:'a[,]) i j =
        let x1 = Array2D.base1 ar
        let x2 = Array2D.length1 ar
        let y1 = Array2D.base2 ar
        let y2 = Array2D.length2 ar
        if (i >= x1 && i < x2 && j >= y1 && j < y2) then Some ar.[i,j]
        else None

let rec evolve1 wa =
    let waArray = unwrap wa
    let resolve i j el =
        let adjacentAndSelf = waArray.[i-1..i+1, j-1..j+1] |> Seq.cast<Cell>
        match el with
        | Floor -> Floor
        | SeatEmpty ->
            let toOccupy =
                adjacentAndSelf
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.isEmpty
            if (toOccupy) then SeatOccupied else SeatEmpty
        | SeatOccupied ->
            let toFree =
                adjacentAndSelf
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.length
                >= 5
            if (toFree) then SeatEmpty else SeatOccupied
    let newState = Array2D.mapi resolve waArray
    if (Operators.compare newState waArray = 0)
    then wa
    else WaitingArea newState |> evolve1
    
let puzzle11_1 =
    input |> create |> evolve1
    |> unwrap |> Seq.cast<Cell>
    |> Seq.filter ((=) SeatOccupied)
    |> Seq.length


let allDirections = Seq.allPairs (seq {-1; 0; 1}) (seq {-1; 0; 1}) |> Seq.except (seq {0, 0})
                    |> Seq.toArray
let rec evolve2 wa =
    let waArray = unwrap wa
    let collectInDirection x y xDir yDir =
        Seq.unfold (fun (xState, yState) ->
                        let newPoint = xState + xDir, yState + yDir
                        newPoint
                        ||> Array2D.tryGet waArray
                        |> Option.map (fun el -> el, newPoint)
                        ) (x, y)
                
    let resolve i j el =
        let visible() =
            allDirections
            |> Array.choose (fun (xDir, yDir) -> collectInDirection i j xDir yDir
                                                |> Seq.filter ((<>)Floor)
                                                |> Seq.tryHead)
            
        match el with
        | Floor -> Floor
        | SeatEmpty ->
            let toOccupy =
                visible()
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.isEmpty
            if (toOccupy) then SeatOccupied else SeatEmpty
        | SeatOccupied ->
            let toFree =
                visible()
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.length
                >= 5
            if (toFree) then SeatEmpty else SeatOccupied
    let newState = Array2D.mapi resolve waArray
    if (Operators.compare newState waArray = 0)
    then wa
    else WaitingArea newState |> evolve2


let puzzle11_2 =
    input |> create |> evolve2
    |> unwrap |> Seq.cast<Cell>
    |> Seq.filter ((=) SeatOccupied)
    |> Seq.length

