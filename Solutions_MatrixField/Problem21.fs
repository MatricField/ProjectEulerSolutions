﻿//https://projecteuler.net/problem=21

(*
 *Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 *If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 *
 *For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 *Evaluate the sum of all the amicable numbers under 10000.
*)

module Problem21

module method1 =
    let sumOfPropDivisor n =
        let getListToSum (fctr:bigint, exp) =
            [0..exp]
            |>List.map (fun ex -> fctr**ex)

        let sumPlusN = 
            Math.Integer.factorizeBigint n
            |>List.map getListToSum
            |>List.map List.sum
            |>List.reduce (*)

        sumPlusN - n

    let isAmiNum n =
        let d = sumOfPropDivisor n
        if n <> d then
            if sumOfPropDivisor d = n then true else false
        else false

    let amiChooser = function
        |x when x |> isAmiNum -> Some x
        |_ -> None

let main argv = 
    [|2I..10000I|]
    |>Array.Parallel.choose method1.amiChooser
    |>Array.sum
    |>printfn "sum is %A"

    0 // return an integer exit code
