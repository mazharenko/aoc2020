#if !INTERACTIVE
module Puzzle1
#endif

open System
open System.IO

let input =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "puzzle1.txt"))
        .Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int

(*
Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together
produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if
you multiply them together?
*)
let puzzle1 =
    let input = Set.ofSeq input
    let found1 =
        input
        |> Seq.find (fun number -> Set.contains (2020 - number) input)
    let found2 = 2020 - found1;
    found1 * found2
 
#if INTERACTIVE
puzzle1
#endif

(*
The Elves in accounting are thankful for your help; one of them even offers you a starfish coin
they had left over from a past vacation. They offer you a second one if you can find three
numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675.
Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
*)
let puzzle1_2 =
    let crossproduct l1 l2 l3 =
          seq { for el1 in l1 do
                  for el2 in l2 do
                      for el3 in l3 do
                        yield el1, el2, el3 }
          
    let cartesian = crossproduct input input input
    
    let found1, found2, found3 = cartesian |> Seq.find (fun (l1,l2,l3) -> 2020 = l1 + l2 + l3)
    found1 * found2 * found3

#if INTERACTIVE
puzzle1_2
#endif
