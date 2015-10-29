//https://projecteuler.net/problem=67

(*
 *By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
 *
 *3
 *7 4
 *2 4 6
 *8 5 9 3
 *
 *That is, 3 + 7 + 4 + 9 = 23.
 *
 *Find the maximum total from top to bottom in triangle.txt
*)

open Problem18

let solve _ =
    System.IO.File.ReadAllLines("triangle.txt")
    |>Array.Parallel.map (Misc.Text.ListParser.breakWhiteSpace)
    |>Method1.makeHeap
    |>Method1.maxPathSumOf

[<EntryPoint>]
let main argv = 
    let (r,lst),t = Misc.Chrono.timeQuick solve ()
    printfn "The sum is %d, time used: %Ams" r t.TotalMilliseconds
    printfn "%A" lst
    0 // return an integer exit code
