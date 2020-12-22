//
// https://adventofcode.com/2020/day/22
//

#if !INTERACTIVE
module Puzzle22
#else
#load "common.fs"
#r "nuget:FSharpx.Collections, Version=2.1.3"
#endif

open System
open System.IO
open common
open FSharpx.Collections

let (inputDeck1, inputDeck2) =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "puzzle22.txt"))
        .Split([|"\n\n"; "Player 1:"; "Player 2:"|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun sdeck -> sdeck.Split("\n", StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Queue.ofSeq)
    |> (fun ar -> ar.[0], ar.[1])
    
module Part1 = 
    let rec gameRounds deck1 deck2 =
        if (Queue.isEmpty deck1 || Queue.isEmpty deck2) then (deck1, deck2)
        else
            let h1 = Queue.head deck1
            let h2 = Queue.head deck2
            if (h1 > h2)
            then
                let newDeck1 = deck1 |> Queue.tail |> Queue.conj h1 |> Queue.conj h2
                let newDeck2 = deck2 |> Queue.tail
                gameRounds newDeck1 newDeck2
            else
                let newDeck1 = deck1 |> Queue.tail 
                let newDeck2 = deck2 |> Queue.tail |> Queue.conj h2 |> Queue.conj h1
                gameRounds newDeck1 newDeck2
                
    let answer =
        let (resDeck1, resDeck2) = gameRounds inputDeck1 inputDeck2
        let winnerDeck = if (Queue.isEmpty resDeck1) then resDeck2 else resDeck1
        winnerDeck |> Queue.rev |> Queue.toSeq
        |> Seq.mapi (fun i card -> (i+1) * card)
        |> Seq.sum

module Queue =
    let toArray source =
        source |> Queue.toSeq |> Seq.toArray

module Part2 =
    
    let rec gameRounds deck1 deck2 previousRounds =
        if (Queue.isEmpty deck1 || Queue.isEmpty deck2) then (deck1, deck2)
        elif (Set.contains (deck1 |> Queue.toArray, deck2 |> Queue.toArray) previousRounds) then (deck1, Queue.empty)
        else
            let h1 = Queue.head deck1
            let h2 = Queue.head deck2
            let deck1Rest = Queue.tail deck1
            let deck2Rest = Queue.tail deck2
            let firstWins = 
                if ((Queue.length deck1Rest) >= h1
                    && (Queue.length deck2Rest) >= h2)
                then
                    let newDeck1 = deck1Rest |> Queue.toSeq |> Seq.take h1 |> Queue.ofSeq
                    let newDeck2 = deck2Rest |> Queue.toSeq |> Seq.take h2 |> Queue.ofSeq
                    let (_, subGameDeck2) = gameRounds newDeck1 newDeck2 Set.empty
                    Queue.isEmpty subGameDeck2
                else h1 > h2
            let newPrevious = (Set.add (deck1 |> Queue.toArray, deck2 |> Queue.toArray) previousRounds)
            if (firstWins)
            then
                let newDeck1 = deck1Rest |> Queue.conj h1 |> Queue.conj h2
                let newDeck2 = deck2Rest
                gameRounds newDeck1 newDeck2 newPrevious
            else
                let newDeck1 = deck1Rest
                let newDeck2 = deck2Rest |> Queue.conj h2 |> Queue.conj h1
                gameRounds newDeck1 newDeck2 newPrevious
                
    let answer =
        let (resDeck1, resDeck2) = gameRounds inputDeck1 inputDeck2 Set.empty
        let winnerDeck = if (Queue.isEmpty resDeck1) then resDeck2 else resDeck1
        winnerDeck |> Queue.rev |> Queue.toSeq
        |> Seq.mapi (fun i card -> (i+1) * card)
        |> Seq.sum