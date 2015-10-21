//https://projecteuler.net/problem=10

(*
 *The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *Find the sum of all the primes below two million.
*)

let target = 2000000

module Method1 =

    let task x =
        Math.Integer.Prime.findPrimesBelow x
        |>List.map (fun x-> bigint(x))
        |>List.sum

    let solve _ =
        task target
        

[<EntryPoint>]
let main argv = 
    let (r,t) = Misc.Chrono.time Method1.solve ()
    printfn "%A, %As" r t.TotalSeconds
    0 // return an integer exit code
