//
// https://adventofcode.com/2020/day/19
//

#if !INTERACTIVE
module Puzzle19
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System
open System.IO
open FParsec
open common

let input =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "puzzle19.txt")).Split("\n\n")

let match1 c =
    pchar c >>. preturn ()
    
let mutable parserMap : Map<int, Parser<unit,unit> * Parser<unit,unit> ref> = Map.empty

        
let getOrAddParser num =
    parserMap
    |> Map.tryFind num
    |> Option.map fst
    |> Option.defaultWith (fun () ->
        let (p, pref) = createParserForwardedToRef<unit, unit>()
        parserMap <- Map.add num (p, pref) parserMap
        p
    )

let ruleRef =    
    spaces >>. sepEndBy1 pint32 spaces1
    |>> (fun nums -> nums |> List.map (fun num -> getOrAddParser num))
    |>> (fun parsers -> parsers |> Seq.reduce (>>.))
    <??> "ref"

let orRule =
     ruleRef .>>. ((pstring "|") >>. ruleRef)
    |>> (fun (one, two) -> attempt (choice [attempt one; attempt two]))
    

let ruleMatch =
    (pchar '\"' >>. anyChar .>> pchar '\"')
    |>> match1
    
let rule =
    pint32 .>> pstring ": "
    .>>.
    (attempt ruleMatch <|> attempt orRule <|> attempt ruleRef)
    

let rules =
    input.[0].Split("\n") |> Array.map (run rule)
    
let updateParserMap num p =
    parserMap <-
        Map.change num (
           fun found ->
               let (fp, fref) = match found with | Some x -> x | None -> createParserForwardedToRef<unit, unit>()
               fref := p
               (fp, fref) |> Some
        ) parserMap

rules
|> Array.map (
    function
    | Success(result, _, _) -> result
    | _ -> failwith "not parsed"
)
|> Seq.iter (fun (num, p) -> updateParserMap num p)

    
let (rule0, _) = parserMap.[0]
let rule0TillEnd = rule0 .>> eof

let strings = input.[1].Split("\n", StringSplitOptions.RemoveEmptyEntries)


let puzzle19_1 =
    strings
    |> Array.map (run rule0TillEnd)
    |> Array.filter (
        function
        | Success(_, _, _) -> true
        | _ -> false     
    ) |> Array.length

let count c p =
    Seq.replicate c p
    |> Seq.reduce (>>.)
    
// it's all bad with recursive rules.
// 8: 42 | 42 8
// 42: "a"
// 0: 8
// in the set of rules above rule 8 would succeed on the first symbol in
// "aaaaaa", and it's unconvenient to specify what should go after 14 or 14 8.
// in this case it must be eof, but in case of 0: 8 13 it is 13.
// so, the solution for now is to just interpret this two rules and
// reword them in code statically.
// 8: 42 | 42 8 stands for any number of rule 42 (above zero) 
// 11: 42 31 | 42 11 31 stands for any (>0) number of 42 after which go the same number of 31
// however, we can't just apply this "any number" parser, because it would eagerly feed the input while
// the input satisfies the parser. the opposite also won't work.
// so, we're gonna build different versions of rule0 based on different versions of rule8 and rule 11
// and check the string against each of them
let update8and11 c8 c11 =
    [|
        8, count c8 (getOrAddParser 42) >>. preturn ()
        11, (count c11 (getOrAddParser 42)) >>. (count c11 (getOrAddParser 31)) 
    |]
    |> Seq.iter (fun (num, p) -> updateParserMap num p)

let puzzle19_2 =
    ([|1..100|], [|1..100|])
    ||> Array.allPairs
    |> Array.collect (
        fun (c8, c11) ->
            update8and11 c8 c11
            
            strings |> Array.map (fun x -> x, run rule0TillEnd x)
    )
    |> Array.choose (
        fun (s, res) ->
            match res with 
            | Success(_, _, _) -> Some s
            | _ -> None
    )
    |> Array.distinct
    |> Array.length