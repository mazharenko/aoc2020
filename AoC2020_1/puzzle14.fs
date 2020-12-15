//
// https://adventofcode.com/2020/day/14
//

#if !INTERACTIVE
module Puzzle14
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif

open System
open System.Collections
open System.IO
open System.Numerics
open common
open FParsec


let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle14.txt")
    |> readLines
    
type Set = {address: int; value: int64}
/// for      XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
///                                       |    |
/// value =  000000000000000000000000000001000000
///                                       |    |
/// mask =   111111111111111111111111111110111101
type Mod = {value: int64; mask: int64}

let applyMod n modif =
    // modif.value =  000000000000000000000000000001000000
    // modif.mask =   111111111111111111111111111110111101
    // we are about to set bits from value at positions
    // which are not set for the mask
    // so, this applies all '1'
    (n ||| modif.value)
    // mask.mask | mask.value =
    //               111111111111111111111111111111111101
    // and then applies all '0' 
    &&& (modif.mask ||| modif.value)

type Command =
    | SetCommand of Set
    | MaskCommand of Mod

let program =
    let mask (s:string) = {
        //    XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        // -> 000000000000000000000000000001000000
        value = Convert.ToInt64(s.Replace("X", "0"), 2)        
        //    XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        // -> XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX1X
        // -> 111111111111111111111111111110111101
        mask = ~~~Convert.ToInt64(s.Replace('0', '1').Replace("X", "0"), 2)
    }
    let modCommand =
        pstring "mask"
        >>. spaces >>. pstring "=" >>. spaces
        >>. parray 36 (anyOf "01X") |>> (fun chars -> new string(chars)) |>> mask
        |>> MaskCommand
        .>> eof
    let setCommand =
        pstring "mem["
        >>. pint32
        .>>. (pstring "]"
        >>. spaces >>. pstring "=" >>. spaces
        >>. pint64
        .>> eof) |>> (fun (addr, value) -> {address = addr; value = value}) |>> SetCommand
    let command = modCommand <|> setCommand
    input |> Seq.map (run command)
    |> Seq.map (function | Success(result, _, _) -> result | _ -> failwith "not parsed")
    |> Array.ofSeq
    
module Part1 =
    
    type ExecutionState = {i: int; commands: Command[]; modif: Mod; memory: Map<int, int64>}
        with static member create commands = {i = 0; commands = commands; modif = {value = 0L; mask = 0L}; memory = Map.empty}

    let rec execute state =
        let {i = i; commands = commands; modif = modif; memory = memory} = state
        if (i >= commands.Length) then state
        else
            match commands.[i] with
            | MaskCommand m -> execute {state with i = i + 1; modif = m}
            | SetCommand s ->
                let modified = applyMod s.value modif
                execute {state with i = i + 1; memory = Map.add s.address modified memory}
                
    let answer =
        let resultState = execute (ExecutionState.create program)
        resultState.memory |> Map.toSeq |> Seq.sumBy snd

module Part2 =
    
    type ExecutionState = {i: int; commands: Command[]; modif: Mod; floatingValues: int64 list; memory: Map<int64, int64>}
        with static member create commands = {i = 0; commands = commands; modif = {value = 0L; mask = 0L}; floatingValues = List.empty; memory = Map.empty}
    let rec execute state =
        let {i = i; commands = commands; modif = modif; floatingValues = floating; memory = memory} = state
        if (i >= commands.Length) then state
        else
            match commands.[i] with
            | MaskCommand m ->
                let lower = int (m.mask &&& 0xFFFFFFFFL) // lower 32 bits to fit int32
                let higher = int ((m.mask >>> 32) &&& 0xFL) // higher 4 bits
                let ba = BitArray [|lower; higher|]
                
                // each '1' bit will make the fun to fork in two branches with two values of the bit
                let rec alternateBits (bitArray:BitArray) i =
                    if (i >= bitArray.Length) then Seq.singleton bitArray
                    elif (not bitArray.[i]) then alternateBits bitArray (i + 1)
                    else
                        let bitCopyWithZero = bitArray.Clone() :?> BitArray
                        bitCopyWithZero.Set(i, false)
                        seq {
                            alternateBits bitArray (i + 1)
                            alternateBits bitCopyWithZero (i + 1)
                        }
                        |> Seq.concat
                
                let floatingValues =
                    alternateBits ba 0
                    |> Seq.map (fun ba ->
                            let ar = Array.zeroCreate 2
                            ba.CopyTo(ar, 0)
                            (ar.[1] |> uint32 |> int64 <<< 32)
                            ||| (ar.[0] |> uint32 |> int64) 
                        )
                    |> List.ofSeq
                
                // for mod mask  000000000000000000000000000001000010
                // there are 2 floating bits
                // which can have 4 different values: 0, 1, 2, 3, or 00, 01, 10, 11
                //               000000000000000000000000000001000010
                //               000000000000000000000000000001000000
                //               000000000000000000000000000000000010
                //               000000000000000000000000000000000000
                // but first we'll have to apply the modif itself
                let defaultMod = {value = m.value; mask = ~~~m.mask}
                execute {state with i = i + 1; modif = defaultMod; floatingValues = floatingValues}
            | SetCommand s ->
                // for address        000000000000000000000000000000101010
                // and modif.value    000000000000000000000000000000100100
                // and modif.mask     111111111111111111111111111110111101
                // which represents input mask
                //                    00000000000000000000000000000X1001X0
                // we apply modif to get one part of the modified address
                //                    000000000000000000000000000000101100
                let addr1 = applyMod (int64 s.address) modif
                let newMemory =
                // bits on 'X' positions are alternated
                //                    00000000000000000000000000000X1001X0
                //
                //                    000000000000000000000000000001000010
                //                    000000000000000000000000000001000000
                //                    000000000000000000000000000000000010
                //                    000000000000000000000000000000000000
                // addr1 has this bits unset. so | is enough
                    List.fold
                        (fun mem fl ->
                            Map.add (addr1 ||| fl) s.value mem)
                        memory
                        floating
                execute {state with i = i + 1; memory = newMemory}
                    
                 
    let answer =
        let resultState = execute (ExecutionState.create program)
        resultState.memory |> Map.toSeq |> Seq.sumBy snd
            