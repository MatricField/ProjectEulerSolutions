﻿//https://projecteuler.net/problem=16

(*
 *215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

 *What is the sum of the digits of the number 21000?
*)

module Problem16

module Method1=
    let solve () = 
        let a = 2I**1000
        a.ToString().ToCharArray()
        |>Array.Parallel.map (fun x-> string(x))
        |>Array.Parallel.map (bigint.Parse)
        |>Array.sum

let main argv = 
    printfn "%A" <| Method1.solve()
    0 // return an integer exit code
