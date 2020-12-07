#if !INTERACTIVE
module Puzzle7
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System.IO
open System.Text.RegularExpressions
open FParsec
open common

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle7.txt")
    |> readLines
    |> Array.ofSeq
    
let parseLine line =
    let swap (x, y) = y, x
    let p =
        manyCharsTill anyChar (pstring " bags contain ")
        .>>.
        (
            pstring "no other bags" >>. preturn []
            <|>
            sepBy (pint32 .>> spaces .>>. manyCharsTill anyChar (pstring " bags" <|> pstring " bag") |>> swap) (pstring ", ")
        )
    match run p line with
    | Success(result, _, _) -> result
    | _ -> failwith "not parsed"
    
let containingMap =
    input
    |> Seq.map parseLine
    |> Seq.map (fun (contained, containing) -> contained, Map.ofList containing)
    |> Map.ofSeq
    
let containedMap =
    let folder map (key,value) =
        let mapValue x = match x with | Some xx -> Some (Set.add value xx) | None -> Some (Set.singleton value)
        Map.change key mapValue map
    input
    |> Seq.map parseLine
    |> Seq.collect (fun (containing, contained) -> contained |> List.map (fun (color, _) -> color, containing))
    |> Seq.fold folder Map.empty
    
let rec findContaining colorContained =
    containedMap
    |> Map.tryFind colorContained
    |> Option.map (fun containing -> Seq.collect findContaining containing
                                   |> Set.ofSeq
                                   |> Set.union containing)
    |> Option.defaultValue Set.empty

let rec findContained colorContaining =
    let folder accMap map =
        let addOrAdd key value map =
            Map.change key (fun maybeFound -> maybeFound |> Option.map ((+) value >> Some) |> Option.defaultValue (Some value)) map
        Map.fold (fun acc key value -> addOrAdd key value acc) accMap map
    
    containingMap
    |> Map.tryFind colorContaining
    |> Option.map (fun contained -> contained
                                    |> Seq.map (fun x -> findContained x.Key |> Map.map (fun _ count -> x.Value * count))
                                    |> Seq.fold folder contained
                        )
    |> Option.defaultValue Map.empty

let puzzle7_1 = findContaining "shiny gold" |> Set.count
let puzzle7_2 = findContained "shiny gold" |> Map.toSeq |> Seq.sumBy snd
