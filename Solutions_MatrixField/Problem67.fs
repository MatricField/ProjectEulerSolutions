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

module Problem67

open Problem18

let data = 
    System.IO.File.ReadAllLines("triangle.txt")
    |>Array.Parallel.map (Misc.Text.ListParser.breakWhiteSpace)
    |>Method1.makeHeap

let solve () =
    data
    |>Method1.maxPathSumOf

let solve1_1 () =
    data
    |>Method1_1.maxPathSumOf


let main argv = 
//    let r,t = Misc.Chrono.timeQuick solve ()
//    printfn "The sum is %d, time used: %Ams" r t.TotalMilliseconds
//    let r1,t1 = Misc.Chrono.timeQuick solve1_1 ()
//    printfn "The sum is %d, time used: %Ams" r1 t1.TotalMilliseconds
    let t1 = 
        [
            for _ in 1..1000 do
                let _, t = Misc.Chrono.time solve ()
                yield t.TotalMilliseconds
        ]
        |>List.average

    let t2 = 
        [
            for _ in 1..1000 do
                let _, t = Misc.Chrono.time solve1_1 ()
                yield t.TotalMilliseconds
        ]
        |>List.average

    printfn "t1 = %f, t2 = %f" t1 t2

    0 // return an integer exit code
