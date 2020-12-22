//
// https://adventofcode.com/2020/day/18
//

#if !INTERACTIVE
module Puzzle18
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System.IO
open common
open FParsec

let input = readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle18.txt"))

type Operation = (int64 -> int64 -> int64)
    
type Expression =
    | Value of int64
    | Computation of Expression * Operation * Expression
    
let rec calculate expr =
    match expr with
    | Value v -> v
    | Computation (op1, op, op2) -> op (calculate op1) (calculate op2)

    
let pValue : Parser<Expression, unit> = pint64 |>> Value

let pPlus = pchar '+' >>. preturn (+)
let pTimes = pchar '*' >>. preturn (*)

//     1 (+ 2) (* 3) (+ 4)
// =>  [(+ 4); (* 3); (+ 2)]    1
// =>   +4     *3      +2       1
// =>   +4     *3          1+2
// =>   +4           (1+2)*3
// =>       ((1+2)*3)+4      
let buildComputation (firstExpression, restOperations) =
    let rec build l : Expression =
        match l with
        | [] -> firstExpression
        | (op, e)::tail ->  Computation ((build tail), op, e)
    restOperations |> List.rev |> build

module Part1 =
    let (pIntExpression, pIntExpressionRef) = createParserForwardedToRef<Expression, unit>()

    let pComputation =
        (pIntExpression .>> spaces) .>>. many ((pPlus <|> pTimes) .>>. (spaces >>. pIntExpression .>> spaces))
        |>> buildComputation
        
    pIntExpressionRef :=
        pValue <|> (pchar '(' >>. pComputation .>> pchar ')')
        
    let pExpression =
        attempt (pValue .>> eof)
        <|>
        pComputation .>> eof
        
    let parse expr =
        match run pExpression expr with
        | Success(result, _, _) -> result
        | _ -> failwith "not parsed"

    let answer = input
                 |> Seq.map (parse >> calculate)
                 |> Seq.sum
    
module Part2 =
    let (pIntExpression, pIntExpressionRef) = createParserForwardedToRef<Expression, unit>()

    let pOp pOperation pOperand =
        (pOperand .>> spaces) .>>. many (pOperation .>>. (spaces >>. pOperand .>> spaces))
            |>> buildComputation
    
    let pComputation =
        pIntExpression
        |> pOp pPlus 
        |> pOp pTimes
    
    pIntExpressionRef :=
        pValue <|> (pchar '(' >>. pComputation .>> (pchar ')'))
        
    let pExpression =
        attempt (pValue .>> eof)
        <|>
        pComputation .>> eof
        
    let parse expr =
        match run pExpression expr with
        | Success(result, _, _) -> result
        | _ -> failwith "not parsed"
        
    let answer = input
                 |> Seq.map (parse >> calculate)
                 |> Seq.sum