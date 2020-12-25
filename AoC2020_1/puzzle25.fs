//
// https://adventofcode.com/2020/day/25
//

#if !INTERACTIVE
module Puzzle25
#endif


// input
let cardPublic = 12232269L
let doorPublic = 19452773L

let loopStep value subject =
    (value * subject) % 20201227L

let loop loopSize initValue subject =
    [1..loopSize]
    |> List.fold (fun value i -> loopStep value subject) initValue

let loopSize init loopResult =
    let mutable i = 0
    let mutable value = init
    while (value <> loopResult) do
        value <- loopStep value 7L
        i <- i + 1
    i

let cardLoopSize = loopSize 1L cardPublic
let doorLoopSize = loopSize 1L doorPublic

let answer = loop doorLoopSize 1L cardPublic


