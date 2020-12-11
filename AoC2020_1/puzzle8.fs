//
// https://adventofcode.com/2020/day/8
//


#if !INTERACTIVE
module Puzzle8
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System.IO
open common
open FParsec

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle8.txt")
    |> readLines
    |> Array.ofSeq
    
type Command =
    | Jmp of int
    | Nop of int
    | Acc of int
let pcommand =
    (pstring "jmp" >>. spaces1 >>. pint32 |>> Jmp)
    <|>
    (pstring "nop" >>. spaces1 >>. pint32 |>> Nop)
    <|>
    (pstring "acc" >>. spaces1 >>. pint32 |>> Acc)
    

let program =
    input
    |> Array.map (run pcommand)
    |> Array.map (function 
                | Success(result, _, _) -> result
                | _ -> failwith "not parsed")
    
type ExitCode =
    | Complete
    | Loop
    
type ReplaceCommand = Command -> (Command * bool)[]
    

let doNotFork command = None

let rec executei (program: Command[]) i state executed commandFork =
    if (program.Length = i) then [state, Complete]
    else if Set.contains i executed then [state, Loop]
    else
        let newExecuted = Set.add i executed
        let executeWithFork =
            function
            | Nop _ -> executei program (i + 1) state newExecuted
            | Acc acc -> executei program (i + 1) (state + acc) newExecuted
            | Jmp jmp -> executei program (i + jmp) state newExecuted
        let fork = commandFork program.[i]
        (
             match fork with
             | Some c -> executeWithFork c doNotFork
             | None -> []
        ) @ (
            executeWithFork program.[i] commandFork
        )
    
let execute() =
    executei program 0 0 Set.empty doNotFork
    |> List.exactlyOne

let executeWithChanges() =
    let fork c =
        match c with
        | Nop arg -> Jmp arg |> Some
        | Jmp arg -> Nop arg |> Some
        | _ -> None
    executei program 0 0 Set.empty fork

let puzzle8_1 =
    fst (execute())
    
let puzzle8_2 =
    executeWithChanges()
    |> List.filter (fun (_, code) -> code = Complete)
    |> List.exactlyOne
    |> fst
    