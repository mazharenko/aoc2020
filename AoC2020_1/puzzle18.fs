//
// https://adventofcode.com/2020/day/18
//

#if !INTERACTIVE
module Puzzle18
#else
#load "common.fs"
#r "nuget:Farkle, Version=6.3.2"
#endif

open System.IO
open Farkle
open Farkle.Builder
open common

let input = readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle18.txt"))

open Farkle.Builder.OperatorPrecedence

let number = Terminals.genericUnsigned<uint64> "Number"
let expression = nonterminal "Expression"
expression.SetProductions(
    !@ number |> asIs,
    !@ expression .>> "+" .>>. expression => (fun x1 x2 -> x1 + x2),
    !@ expression .>> "*" .>>. expression => (fun x1 x2 -> x1 * x2),
    !& "(" .>>. expression .>> ")" |> asIs
)

module Part1 =
    
    let private opScope =
        OperatorScope(
            LeftAssociative("+", "-", "*", "/")
        )
        
    let private parser = DesigntimeFarkle.withOperatorScope opScope expression |> RuntimeFarkle.build
        
    let answer =
        input
        |> Seq.map (RuntimeFarkle.parseString parser)
        |> Seq.map (function | Ok value -> value | Error error -> failwith (error.ToString()))
        |> Seq.sum
        

module Part2 =
    
    let private opScope =
        OperatorScope(
            LeftAssociative("*", "/"),
            LeftAssociative("+", "-")
        )
        
    let private parser = DesigntimeFarkle.withOperatorScope opScope expression |> RuntimeFarkle.build
        
    let answer =
        input
        |> Seq.map (RuntimeFarkle.parseString parser)
        |> Seq.map (function | Ok value -> value | Error error -> failwith (error.ToString()))
        |> Seq.sum