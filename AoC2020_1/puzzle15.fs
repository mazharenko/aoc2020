//
// https://adventofcode.com/2020/day/15
//

#if !INTERACTIVE
module Puzzle15
#else
#load "common.fs"
#endif

let input = [|8;13;1;0;18;9|]

type State = { lastSpoken: int; lastTimes:Map<int, int list> }

let turn state i =
    if (i < input.Length)
    then 
        let num = input.[i]
        { lastSpoken = num; lastTimes = Map.add num [i] state.lastTimes }
    else
        let num =
            match state.lastTimes |> Map.find state.lastSpoken with
            |  last::lastButOne::_ -> last - lastButOne
            | _ -> 0
        let newLastTimes =
            match state.lastTimes |> Map.tryFind num with
            | Some l -> Map.add num (i::l) state.lastTimes
            | None -> Map.add num [i] state.lastTimes
        { lastSpoken = num; lastTimes = newLastTimes }
        
let puzzle1Answer =
    let state = {lastSpoken = 0; lastTimes = Map.empty}
    let state2020 = [0..(2020-1)] |> List.fold turn state
    state2020.lastSpoken
    
let puzzle2Answer =
    let state = {lastSpoken = 0; lastTimes = Map.empty}
    let state30000000 = [0..(30000000-1)] |> List.fold turn state
    state30000000.lastSpoken


