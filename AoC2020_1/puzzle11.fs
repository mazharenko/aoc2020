#if !INTERACTIVE
module Puzzle11
#else
#load "common.fs"
#endif

open System.IO
open common

type Cell =
| Floor
| SeatEmpty
| SeatOccupied

type WaitingArea = WaitingArea of Cell[,]

let unwrap =
    function
    | WaitingArea cells -> cells
    
let create chars =
    chars
    |> Array2D.map (fun c -> match c with | '#' -> SeatOccupied | 'L' -> SeatEmpty | _ -> Floor)
    |> WaitingArea
    
let copy (wa: WaitingArea) =
    wa |> unwrap |> Array2D.copy |> WaitingArea
    

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "puzzle11.txt")
    |> read2d
    
module Array2D =
    let tryGet (ar:'a[,]) i j =
        let x1 = Array2D.base1 ar
        let x2 = Array2D.length1 ar
        let y1 = Array2D.base2 ar
        let y2 = Array2D.length2 ar
        if (i >= x1 && i < x2 && j >= y1 && j < y2) then Some ar.[i,j]
        else None


//Part1
//Your plane lands with plenty of time to spare. The final leg of your journey is a ferry that goes
//directly to the tropical island where you can finally start your vacation. As you reach the waiting
//area to board the ferry, you realize you're so early, nobody else has even arrived yet!
//
//By modeling the process people use to choose (or abandon) their seat in the waiting area, you're pretty
// sure you can predict the best place to sit. You make a quick map of the seat layout (your puzzle input).
//
//The seat layout fits neatly on a grid. Each position is either floor (.), an empty seat (L), or an occupied
//seat (#). For example, the initial seat layout might look like this:
//
//L.LL.LL.LL
//LLLLLLL.LL
//L.L.L..L..
//LLLL.LL.LL
//L.LL.LL.LL
//L.LLLLL.LL
//..L.L.....
//LLLLLLLLLL
//L.LLLLLL.L
//L.LLLLL.LL
//Now, you just need to model the people who will be arriving shortly. Fortunately, people are entirely
//predictable and always follow a simple set of rules. All decisions are based on the number of occupied
//seats adjacent to a given seat (one of the eight positions immediately up, down, left, right, or diagonal
//from the seat). The following rules are applied to every seat simultaneously:
//
//If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
//If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
//Otherwise, the seat's state does not change.
//Floor (.) never changes; seats don't move, and nobody sits on the floor.
//
//After one round of these rules, every seat in the example layout becomes occupied:
//
//#.##.##.##
//#######.##
//#.#.#..#..
//####.##.##
//#.##.##.##
//#.#####.##
//..#.#.....
//##########
//#.######.#
//#.#####.##
//After a second round, the seats with four or more occupied adjacent seats become empty again:
//
//#.LL.L#.##
//#LLLLLL.L#
//L.L.L..L..
//#LLL.LL.L#
//#.LL.LL.LL
//#.LLLL#.##
//..L.L.....
//#LLLLLLLL#
//#.LLLLLL.L
//#.#LLLL.##
//This process continues for three more rounds:
//
//#.##.L#.##
//#L###LL.L#
//L.#.#..#..
//#L##.##.L#
//#.##.LL.LL
//#.###L#.##
//..#.#.....
//#L######L#
//#.LL###L.L
//#.#L###.##
//#.#L.L#.##
//#LLL#LL.L#
//L.L.L..#..
//#LLL.##.L#
//#.LL.LL.LL
//#.LL#L#.##
//..L.L.....
//#L#LLLL#L#
//#.LLLLLL.L
//#.#L#L#.##
//#.#L.L#.##
//#LLL#LL.L#
//L.#.L..#..
//#L##.##.L#
//#.#L.LL.LL
//#.#L#L#.##
//..L.L.....
//#L#L##L#L#
//#.LLLLLL.L
//#.#L#L#.##
//At this point, something interesting happens: the chaos stabilizes and further applications of these rules
//cause no seats to change state! Once people stop moving around, you count 37 occupied seats.
//
//Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many s
//eats end up occupied?
let rec evolve1 wa =
    let waArray = unwrap wa
    let resolve i j el =
        let adjacentAndSelf = waArray.[i-1..i+1, j-1..j+1] |> Seq.cast<Cell>
        match el with
        | Floor -> Floor
        | SeatEmpty ->
            let toOccupy =
                adjacentAndSelf
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.isEmpty
            if (toOccupy) then SeatOccupied else SeatEmpty
        | SeatOccupied ->
            let toFree =
                adjacentAndSelf
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.length
                >= 5
            if (toFree) then SeatEmpty else SeatOccupied
    let newState = Array2D.mapi resolve waArray
    if (Operators.compare newState waArray = 0)
    then wa
    else WaitingArea newState |> evolve1
    
let puzzle11_1 =
    input |> create |> evolve1
    |> unwrap |> Seq.cast<Cell>
    |> Seq.filter ((=) SeatOccupied)
    |> Seq.length

//Part 2
//As soon as people start to arrive, you realize your mistake. People don't just care about adjacent seats
//- they care about the first seat they can see in each of those eight directions!
//
//Now, instead of considering just the eight immediately adjacent seats, consider the first seat in each of
//those eight directions. For example, the empty seat below would see eight occupied seats:
//
//.......#.
//...#.....
//.#.......
//.........
//..#L....#
//....#....
//.........
//#........
//...#.....
//The leftmost empty seat below would only see one empty seat, but cannot see any of the occupied ones:
//
//.............
//.L.L.#.#.#.#.
//.............
//The empty seat below would see no occupied seats:
//
//.##.##.
//#.#.#.#
//##...##
//...L...
//##...##
//#.#.#.#
//.##.##.
//Also, people seem to be more tolerant than you expected: it now takes five or more visible occupied seats
//for an occupied seat to become empty (rather than four or more from the previous rules). The other rules
//still apply: empty seats that see no occupied seats become occupied, seats matching no rule don't change,
//and floor never changes.
//
//Given the same starting layout as above, these new rules cause the seating area to shift around as follows:
//
//L.LL.LL.LL
//LLLLLLL.LL
//L.L.L..L..
//LLLL.LL.LL
//L.LL.LL.LL
//L.LLLLL.LL
//..L.L.....
//LLLLLLLLLL
//L.LLLLLL.L
//L.LLLLL.LL
//#.##.##.##
//#######.##
//#.#.#..#..
//####.##.##
//#.##.##.##
//#.#####.##
//..#.#.....
//##########
//#.######.#
//#.#####.##
//#.LL.LL.L#
//#LLLLLL.LL
//L.L.L..L..
//LLLL.LL.LL
//L.LL.LL.LL
//L.LLLLL.LL
//..L.L.....
//LLLLLLLLL#
//#.LLLLLL.L
//#.LLLLL.L#
//#.L#.##.L#
//#L#####.LL
//L.#.#..#..
//##L#.##.##
//#.##.#L.##
//#.#####.#L
//..#.#.....
//LLL####LL#
//#.L#####.L
//#.L####.L#
//#.L#.L#.L#
//#LLLLLL.LL
//L.L.L..#..
//##LL.LL.L#
//L.LL.LL.L#
//#.LLLLL.LL
//..L.L.....
//LLLLLLLLL#
//#.LLLLL#.L
//#.L#LL#.L#
//#.L#.L#.L#
//#LLLLLL.LL
//L.L.L..#..
//##L#.#L.L#
//L.L#.#L.L#
//#.L####.LL
//..#.#.....
//LLL###LLL#
//#.LLLLL#.L
//#.L#LL#.L#
//#.L#.L#.L#
//#LLLLLL.LL
//L.L.L..#..
//##L#.#L.L#
//L.L#.LL.L#
//#.LLLL#.LL
//..#.L.....
//LLL###LLL#
//#.LLLLL#.L
//#.L#LL#.L#
//Again, at this point, people stop shifting around and the seating area reaches equilibrium. Once this
//occurs, you count 26 occupied seats.
//
//Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium
//is reached, how many seats end up occupied?
let allDirections = Seq.allPairs (seq {-1; 0; 1}) (seq {-1; 0; 1}) |> Seq.except (seq {0, 0})
                    |> Seq.toArray
let rec evolve2 wa =
    let waArray = unwrap wa
    let collectInDirection x y xDir yDir =
        Seq.unfold (fun (xState, yState) ->
                        let newPoint = xState + xDir, yState + yDir
                        newPoint
                        ||> Array2D.tryGet waArray
                        |> Option.map (fun el -> el, newPoint)
                        ) (x, y)
                
    let resolve i j el =
        let visible() =
            allDirections
            |> Array.choose (fun (xDir, yDir) -> collectInDirection i j xDir yDir
                                                |> Seq.filter ((<>)Floor)
                                                |> Seq.tryHead)
            
        match el with
        | Floor -> Floor
        | SeatEmpty ->
            let toOccupy =
                visible()
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.isEmpty
            if (toOccupy) then SeatOccupied else SeatEmpty
        | SeatOccupied ->
            let toFree =
                visible()
                |> Seq.filter ((=) SeatOccupied)
                |> Seq.length
                >= 5
            if (toFree) then SeatEmpty else SeatOccupied
    let newState = Array2D.mapi resolve waArray
    if (Operators.compare newState waArray = 0)
    then wa
    else WaitingArea newState |> evolve2


let puzzle11_2 =
    input |> create |> evolve2
    |> unwrap |> Seq.cast<Cell>
    |> Seq.filter ((=) SeatOccupied)
    |> Seq.length

