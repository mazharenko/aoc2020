//
// https://adventofcode.com/2020/day/1
//

#if !INTERACTIVE
module Puzzle1
#else
#load "common.fs"
#endif

open System.IO
open common

let input =
    readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle1.txt"))
    |> Seq.map int

let puzzle1 =
    let input = Set.ofSeq input
    let found1 =
        input
        |> Seq.find (fun number -> Set.contains (2020 - number) input)
    let found2 = 2020 - found1;
    found1 * found2
 
#if INTERACTIVE
puzzle1
#endif

let puzzle1_2 =
    let crossproduct l1 l2 l3 =
          seq { for el1 in l1 do
                  for el2 in l2 do
                      for el3 in l3 do
                        yield el1, el2, el3 }
          
    let cartesian = crossproduct input input input
    
    let found1, found2, found3 = cartesian |> Seq.find (fun (l1,l2,l3) -> 2020 = l1 + l2 + l3)
    found1 * found2 * found3

#if INTERACTIVE
puzzle1_2
#endif
