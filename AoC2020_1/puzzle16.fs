//
// https://adventofcode.com/2020/day/16
//

#if !INTERACTIVE
module Puzzle16
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System
open System.IO
open FParsec
open common

type Rule = {field: string; ranges: (int*int)[]}


let satisfies value rule =
    rule.ranges
    |> Seq.forAny (fun (rBegin, rEnd) -> rBegin <= value && value <= rEnd)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "puzzle16.txt")).Split("\n\n")
let rules =
    let range = pint32 .>> pchar '-' .>>. pint32
    let rule = many1CharsTill anyChar (pchar ':')
               .>>.
               (spaces >>. sepBy1 range (pstring " or "))
               |>> fun (name, ranges) -> {field = name; ranges = ranges |> Array.ofList}
               
    let ruleSet = sepBy1 rule spaces1 |>> Array.ofList .>> eof
    
    let f = run ruleSet input.[0]
    match f with
    | Success(result, _, _) -> result
    | _ -> failwith "not parsed"
    
let myTicket =
    input.[1].Split("\n").[1].Split(",")
    |> Array.map int
    
let nearby =
    input.[2].Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.skip 1
    |> Seq.map (fun s -> s.Split(",") |> Array.map int)
    |> Seq.toArray
    
    
let puzzle16_1 =
    nearby
    |> Seq.collect id
    |> Seq.filter (fun v ->
        rules |> Array.forall (not << satisfies v)
    )
    |> Seq.sum
       

type State = {matching: (int * Map<string, Rule>) list; solved: Rule[]}

let rec solve state =
    match state.matching with
    | [] -> state
    | list ->
        let (singleRuleIndex, singleRule) =
             list
             |> Seq.filter (fun (i, c) -> c.Count = 1)
             |> Seq.tryHead
             |> Option.map (fun (i, c) -> i, (Seq.exactlyOne c).Value)
             |> Option.get
             
        state.solved.[singleRuleIndex] <- singleRule 
             
        let withoutThatRule =
            list
            |> List.choose (fun (i, c) ->
                                let newMap = Map.remove singleRule.field c
                                if (Map.isEmpty newMap) then None
                                else Some (i, newMap)
                            )
        solve { state with matching = withoutThatRule } 
        

let solvedFields =
    let correctNb =
        nearby |>
        Array.filter (fun nbs ->
                        nbs |> Array.forall (fun v -> rules |> Array.forany (satisfies v))    
        )
    let nb =
        Array2D.init correctNb.Length rules.Length (fun i j -> correctNb.[i].[j])
    let determineCandidates values =    
        rules
        |> Array.filter (fun rule -> values |> Array.forall (fun v -> satisfies v rule))
        |> Array.map (fun x -> x.field, x)
        |> Map.ofArray
    let candidates  =
        [0..(Array2D.length2 nb - 1)]
        |> List.map (fun i -> i, (determineCandidates (nb.[0..,i..i] |> Seq.cast<int> |> Array.ofSeq)))
        
    (solve {matching = candidates; solved = Array.zeroCreate (rules.Length)}).solved
    
let myTickedSolved =
    Array.zip solvedFields myTicket
    
let puzzle16_2 =
    myTickedSolved
    |> Array.filter (fun (rule, _) -> rule.field.StartsWith "departure")
    |> Array.map snd
    |> Array.map int64
    |> Array.reduce (*)
