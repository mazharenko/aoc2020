//
// https://adventofcode.com/2020/day/2
//

#if !INTERACTIVE
module Puzzle2
#else
#load "common.fs"
#endif

open System.IO
open System.Text.RegularExpressions
open common

type rule = { letter: char; min: int; max: int }

let check (rule: rule) (pw: string): bool =
    let count =
        pw
        |> Seq.where (fun c -> c = rule.letter)
        |> Seq.length

    count >= rule.min && count <= rule.max

let parseCase s =
    let m =
        Regex("(?<min>\d+)\-(?<max>\d+) (?<char>\w): (?<pw>\w*)").Match(s)

    let rule =
        { letter = m.Groups.["char"].Value.[0]
          min = int m.Groups.["min"].Value
          max = int m.Groups.["max"].Value }

    let pw = m.Groups.["pw"].Value
    rule, pw

let input =
    readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle2.txt"))
    |> Seq.map parseCase


let puzzle2 =
    input
    |> Seq.where (fun (rule, pw) -> check rule pw)
    |> Seq.length

#if INTERACTIVE
puzzle2
#endif


let check2 (rule: rule) (pw: string): bool =
    (pw.[rule.min - 1] = rule.letter)
    <> (pw.[rule.max - 1] = rule.letter)

let puzzle2_2 =
    input
    |> Seq.where (fun (rule, pw) -> check2 rule pw)
    |> Seq.length

#if INTERACTIVE
puzzle2_2
#endif