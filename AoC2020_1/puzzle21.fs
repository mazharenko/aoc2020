//
// https://adventofcode.com/2020/day/21
//

#if !INTERACTIVE
module Puzzle21
#else
#load "common.fs"
#endif

open System
open System.IO
open common
open common

let input =
    readLines (Path.Combine(__SOURCE_DIRECTORY__, "puzzle21.txt"))
    |> Seq.map (fun s ->
        let split = s.Split(" (contains ")
        let ing = split.[0]
        let allerg = split.[1].TrimEnd(')')
        (ing.Split(" ") |> Set.ofArray), (allerg.Split(", ") |> Set.ofArray)
    ) |> Array.ofSeq
    
let allAllergs =
    input |> Seq.collect snd |> Set.ofSeq
    
let find ar alMap aller =
    let ingredientsPossiblyHavingAller =
        ar
        |> Array.choose (fun (ings, alls) -> if (Set.contains aller alls) then Some ings else None)
        |> (function | [||] -> Set.empty | sets -> Set.intersectMany sets)
    if (Set.count ingredientsPossiblyHavingAller = 1)
    then
        let ingr = ingredientsPossiblyHavingAller |> Set.toArray |> Array.head
        Map.add aller ingr alMap, ar |> Array.map (fun (ings, alls) -> Set.remove ingr ings, Set.remove aller alls)
    else
        alMap, ar
    
let rec ``match`` alMap ar =
    let (alCorMap, afterRemoving) =
        allAllergs
        |> Seq.fold (fun (alMap, ar)  al -> find ar alMap al) (alMap, ar)
        
    if (afterRemoving |> Array.forall (fun (_,x) -> Set.isEmpty x)) then alCorMap, afterRemoving
    else if (Operators.compare ar afterRemoving = 0) then failwith "loop"
    else
        ``match`` alCorMap afterRemoving
        
let (matchedAllergens, _) = ``match`` Map.empty input
    
let puzzle21_1 =
    ``match`` Map.empty input
    |> snd
    |> Seq.map fst
    |> Seq.concat
    |> Seq.length
    
let puzzle21_2 =
    matchedAllergens
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> (fun x -> String.Join(",", x))