//https://projecteuler.net/problem=20

(*
 *n! means n × (n − 1) × ... × 3 × 2 × 1
 *
 *For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
 *and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 *
 *Find the sum of the digits in the number 100!
*)

let TARGET = 100I

module Method1 =
    let rec factorialrec = function
        |x when x=0I -> 1I
        |x when x<0I -> raise (System.ArgumentException "Factorial of negtive interger is undefined")
        |x -> x * factorialrec (x-1I)

    let factorial =
        let rec loop result = function
            |x when x=0I -> result
            |x -> loop (result*x) (x-1I)
        function
        |x when x=0I -> 1I
        |x when x>0I -> loop 1I x
        |_ -> raise (System.ArgumentException "Factorial of negtive interger is undefined")

    let digitSumOf x =
        (factorial x).ToString().ToCharArray()
        |>Array.Parallel.map (fun x-> string(x)|>System.Numerics.BigInteger.Parse)
        |>Array.sum

    let solve () =
        "The sum is: " + (digitSumOf TARGET).ToString()

[<EntryPoint>]
let main argv =
    let (r,t) = Misc.Chrono.timeQuick Method1.solve ()
    printfn "%s Time used: %A ms" r t.TotalMilliseconds
    0 // return an integer exit code
