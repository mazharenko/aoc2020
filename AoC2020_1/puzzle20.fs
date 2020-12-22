//
// https://adventofcode.com/2020/day/20
//

#if !INTERACTIVE
module Puzzle20
#else
#load "common.fs"
#endif

open System
open System.IO
open common

module Tile =
    type Tile = private Tile of int*char[,]
        with override this.ToString() =
                match this with
                | Tile (id, ar) -> let rowStrings =
                                     ar |> Seq.cast<char> |> Seq.splitInto (Array2D.length1 ar)
                                     |> Seq.map (fun row -> String.Join(" ", row))
                                   String.Join("\n", rowStrings)
    let id tile =
        let (Tile (id, _)) = tile
        id

    let private array tile =
        let (Tile (_, ar)) = tile
        ar
    
    let create id ar =
        Tile (id, (Array2D.copy ar))
        
    let toArray tile =
        Array2D.copy (array tile)
        
    let length1 tile =
        let (Tile (_, ar)) = tile
        Array2D.length1 ar
        
    let length2 tile =
        let (Tile (_, ar)) = tile
        Array2D.length2 ar
    
    let left tile =
        ((array tile).[*,0] |> String.fromArray)        
    let top tile =
        ((array tile).[0,*] |> String.fromArray)
    let right tile =
        ((array tile).[*, 9] |> String.fromArray)
    let bottom tile =
        ((array tile).[9, *] |> String.fromArray)
        
    let rotate tile =
        let resolveCoordinatesRotated i j = (array tile |> Array2D.length1) - 1 - j, i
        let (Tile (id, ar)) = tile
        Array2D.init (Array2D.length2 ar) (Array2D.length1 ar)
            (fun i j ->
                let (i', j') = resolveCoordinatesRotated i j
                ar.[i', j'])
        |> create id
    
    let flipi tile =
        let resolveCoordinatesFlipped i j = i, (array tile |> Array2D.length2) - 1 - j
        let (Tile (id, ar)) = tile
        Array2D.init (Array2D.length1 ar) (Array2D.length2 ar)
            (fun i j ->
                let (i', j') = resolveCoordinatesFlipped i j
                ar.[i', j'])
        |> create id
        
    let alternatives tile =
        [|
            tile
            tile |> rotate
            tile |> rotate |> rotate
            tile |> rotate |> rotate |> rotate
            tile |> flipi
            tile |> flipi |> rotate
            tile |> flipi |> rotate |> rotate
            tile |> flipi |> rotate |> rotate |> rotate
        |]
        
let inputTiles =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "puzzle20.txt"))
                    .Split([|"\n\n"; "Tile "; ":"|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.chunkBySize 2
    |> Array.map (fun ar ->
        let id = int ar.[0]
        let tileAr = ar.[1].Split("\n", StringSplitOptions.RemoveEmptyEntries)
        Tile.create id (Array2D.init 10 10 (fun i j -> tileAr.[i].[j])) 
    )
    
let tilesBy by =
    inputTiles
    |> Seq.collect Tile.alternatives
    |> Seq.groupBy by
    |> Seq.map (fun (key, g) -> key, g |> Array.ofSeq)
    |> Map.ofSeq

let tilesByLeft = tilesBy Tile.left
let tilesByRight = tilesBy Tile.right
let tilesByTop = tilesBy Tile.top
let tilesByBottom = tilesBy Tile.bottom
    
type State = {
    map: Map<int*int, Tile.Tile>
    left: Map<int, Tile.Tile>
}
with member this.next point tile =
        if (Map.containsKey point this.map) then failwithf "tile coordinates %O already taken" point
        if (not <| Map.containsKey (Tile.id tile) this.left) then failwithf "tile %i already used" (Tile.id tile)
        { map = Map.add point tile this.map; left = Map.remove (Tile.id tile) this.left}

type SearchResult =
    | Success of State
    | Failure of State

let emptyState = {
    map = Map.empty
    left = inputTiles |> Array.map (fun tile -> Tile.id tile, tile) |> Map.ofArray
}


let rec buildMap (i, j) currentTile (state: State) =
    
    if (state.left |> Map.isEmpty) then state 
    else
        let (+) (t11, t12) (t21, t22) = (t11 + t21, t12 + t22) 
          
        let adjacent p tile =
            [
                (0, -1) + p, Map.find (Tile.top tile) tilesByBottom
                (1, 0) + p, Map.find (Tile.right tile) tilesByLeft
                (-1, 0) + p, Map.find (Tile.left tile) tilesByRight
                (0, 1) + p, Map.find (Tile.bottom tile) tilesByTop
            ]
            
        let g = adjacent (i, j) currentTile
        g
        |> Seq.fold (
            fun s (adjPoint, adj) ->
                if (Map.containsKey adjPoint s.map) then s
                else
                    adj
                    |> Array.filter (fun cand -> (Tile.id currentTile) <> (Tile.id cand))
                    |> function
                        | [||] -> s
                        | [|singleAdj|] -> buildMap adjPoint singleAdj (s.next adjPoint singleAdj)
                        | _ -> failwith "more than one possible adjacent tile"
                
        ) (state)
        
//let startTile = inputTiles.[0]
let startTile = inputTiles.[0] |> Tile.flipi |> Tile.rotate |> Tile.rotate

let mapBuilt = buildMap (0,0) startTile (emptyState.next (0,0) startTile)

let puzzle20_1 =
    let {map = map; left = _} = mapBuilt
    let mini = Map.toSeq map |> Seq.map (fst >> fst) |> Seq.min 
    let minj = Map.toSeq map |> Seq.map (fst >> snd) |> Seq.min    
    let maxi = Map.toSeq map |> Seq.map (fst >> fst) |> Seq.max 
    let maxj = Map.toSeq map |> Seq.map (fst >> snd) |> Seq.max
    (Tile.id map.[(mini, minj)] |> int64)
    * (Tile.id map.[(maxi, minj)] |> int64)
    * (Tile.id map.[(mini, maxj)]  |> int64)
    * (Tile.id map.[(maxi, maxj)]  |> int64)
    
let mapWithoutBorders =
    let {map = map; left = _} = mapBuilt
    let tilesWithoutBorders = map |> Map.map (fun _ tile -> (tile |> Tile.toArray).[1..8,1..8])
    let mini = Map.toSeq map |> Seq.map (fst >> fst) |> Seq.min 
    let minj = Map.toSeq map |> Seq.map (fst >> snd) |> Seq.min    
    let maxi = Map.toSeq map |> Seq.map (fst >> fst) |> Seq.max 
    let maxj = Map.toSeq map |> Seq.map (fst >> snd) |> Seq.max
    let res = Array2D.create ((maxi - mini + 1) * 8) ((maxj - minj + 1) * 8) '?'
    
    tilesWithoutBorders
    |> Map.toSeq
    |> Seq.iter (fun ((i, j), t) -> Array2D.blit t 0 0 res ((j- minj)*8) ((i - mini)*8) 8 8)
    
    res |> Tile.create 0
    
let monster =
    let pattern = 
        [|
            "                  # "
            "#    ##    ##    ###"
            " #  #  #  #  #  #   "
        |]
    Array2D.init pattern.Length pattern.[0].Length (fun i j -> pattern.[i].[j])
    |> Tile.create 1

let mapAlternatives = mapWithoutBorders |> Tile.alternatives
        
let highlightMonsters (map: char[,]) =
    let monsterArray = monster |> Tile.toArray
    let monsterArray1 = monsterArray |> Array2D.toArray
    let isMonster (i, j) =
        let part = map.[i .. i + Array2D.length1 monsterArray - 1, j .. j + Array2D.length2 monsterArray - 1]
        if (Array2D.length1 part <> Array2D.length1 monsterArray || Array2D.length2 part <> Array2D.length2 monsterArray) then false
        else
            let partMonsterMasked = 
                Array.zip monsterArray1 (part |> Array2D.toArray)
                |> Array.map (function | '#', '#' -> '#' | _, _ -> ' ')
            Seq.compareWith Operators.compare partMonsterMasked monsterArray1 = 0
            
    let monsterSetMask = Array2D.map (function | '#' -> Some '●' | _ -> None) monsterArray
    let monsterCoords =
        Seq.allPairs [0..(Array2D.length1 map - 1)] [0 .. (Array2D.length2 map - 1)]
        |> Seq.filter isMonster
        |> Array.ofSeq
    if (monsterCoords.Length = 0) then None
    else
        let res = Array2D.copy map
        monsterCoords
        |> Seq.iter (fun (i, j) ->
                            let maskedPiece =
                                monsterSetMask
                                |> Array2D.mapi (fun mi mj m -> match m with | Some c -> c | None -> map.[i+mi, j+mj])
                            Array2D.blit maskedPiece 0 0 res i j (Array2D.length1 maskedPiece) (Array2D.length2 maskedPiece)  
                        )
        Some res
            
let puzzle20_2 =
    mapAlternatives
    |> Array.map Tile.toArray
    |> Array.choose highlightMonsters
    |> Array.exactlyOne
    |> Seq.cast<char>
    |> Seq.filter ((=)'#')
    |> Seq.length
