﻿//https://projecteuler.net/problem=12

(*
 *The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
 *
 *1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 *
 *Let us list the factors of the first seven triangle numbers:
 *
 * 1: 1
 * 3: 1,3
 * 6: 1,2,3,6
 *10: 1,2,5,10
 *15: 1,3,5,15
 *21: 1,3,7,21
 *28: 1,2,4,7,14,28
 *We can see that 28 is the first triangle number to have over five divisors.
 *
 *What is the value of the first triangle number to have over five hundred divisors?
*)

module Method1 =
    let nextTrangle n prev = prev + n + 1L

    let nDivisorsOf = function
        |1L -> 1
        |n when n>1L ->
            Math.Integer.factorizeL n
            |>List.fold (fun product (_,exp) ->product*(exp+1)) 1
        |_ -> raise (System.ArgumentException "")

    let trangle n =
        let rec loop result prev = function
            |x when x=n -> result
            |x ->
                let this = nextTrangle x prev
                loop (this::result) this (x+1L)
        loop [1L] 1L 1L

    let trangleDivOver n =
        let rec loop prev = function
            |_ when nDivisorsOf prev > n -> prev
            |x ->
                let this = nextTrangle x prev
                loop this (x+1L)
        loop 1L 1L

module Method2 =
    let nDivisorsOf = Method1.nDivisorsOf

    let trangle n =
        n*(n+1L)/2L
        //(n+2)*(n-1)/2 + 1
        //(n^2+n-2)/2 + 1
        //(n^2+n)/2 - 1 + 1
        //n*(n+1)/2

[<EntryPoint>]
let main argv =
    [for i in [1L..10L] do yield Method2.trangle i]
    |>printfn "%A"

    Method1.trangleDivOver 500
    |>printfn "%A"
    0 // return an integer exit code
