//https://projecteuler.net/problem=7

(*
 *By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 *What is the 10 001st prime number?
*)

module Msic =
    let time func vars =
        let t1 = System.DateTime.Now

        let result = func vars

        let t2 = System.DateTime.Now

        (result, t2-t1)

module Method1 =
    let isPrime = Probelm3.Method3.isPrime

    let search number =
        let inc = fun x-> x+1I
        let rec loop result = function
            |n when ((result|>List.length)<number) ->
                match n with
                |_ when (isPrime n result) -> loop (n::result) (inc n)
                |_ -> loop result (inc n)
            |_ -> result
        loop [2I] 2I

    let solve () =
        let number = 10001
        search number
        |>List.head

[<EntryPoint>]
let main argv = 
    let (result,time) = Msic.time Method1.solve ()
    printfn "%A, %As" result time.TotalSeconds
    0 // return an integer exit code