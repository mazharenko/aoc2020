//
// https://adventofcode.com/2020/day/3
//


#if !INTERACTIVE
module Puzzle3
#else
#load "common.fs"
#endif

open System.IO
open common


module Slope =

    [<Struct>]
    type Cell =
        | Empty
        | Tree

    type Location =
        | In of Cell
        | Out

    type Slope = private T of Cell [,]
    let private unwrap =
        function
        | T cells -> cells
        
    let create matrix =
        matrix
        |> Array2D.map (fun c -> match c with | '#' -> Tree | _ -> Empty)
        |> T

    let checkLocation (slope: Slope) x y =
        let cells = unwrap slope
        if (y < 0 || y >= Array2D.length1 cells) then
            Out
        else
            Array2D.get cells y (x % Array2D.length2 cells)
            |> In


let move (fromX, fromY) (right, down) = fromX + right, fromY + down

let moves (fromX, fromY) (right, down) =
    let generateMove (stateX, stateY) =
        let newPoint = move (stateX, stateY) (right, down)
        Some (newPoint, newPoint)
            
    Seq.unfold generateMove (fromX, fromY)


let startPoint = 0, 0
let countTrees slope (right, down) =
    moves startPoint (right, down)
    |> Seq.map (fun (x, y) -> Slope.checkLocation slope x y)
    |> Seq.takeWhile ((<>) Slope.Out)
    |> Seq.filter ((=) (Slope.In Slope.Tree))
    |> Seq.length

let slope =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle3.txt")
    |> read2d
    |> Slope.create
    
let puzzle3_1 =
    countTrees slope (3, 1)


let puzzle3_2 =
    (countTrees slope (1, 1) |> int64)
    * (countTrees slope (3, 1) |> int64)
    * (countTrees slope (5, 1) |> int64)
    * (countTrees slope (7, 1) |> int64)
    * (countTrees slope (1, 2) |> int64)
