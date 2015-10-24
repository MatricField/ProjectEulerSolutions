//https://projecteuler.net/problem=25

(*
 *The Fibonacci sequence is defined by the recurrence relation:
 *
 *Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
 *Hence the first 12 terms will be:
 *
 *F1 = 1
 *F2 = 1
 *F3 = 2
 *F4 = 3
 *F5 = 5
 *F6 = 8
 *F7 = 13
 *F8 = 21
 *F9 = 34
 *F10 = 55
 *F11 = 89
 *F12 = 144
 *The 12th term, F12, is the first term to contain three digits.
 *
 *What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*)

module Method1 =
    let U P Q = Math.Sequence.Lucas.mainLoop 0I 1I P Q

    let fib = U 1I -1I

    let firstFibHasDigit x =
        let mutable result = 0I
        fib (fun count current -> result<-count; current.ToString().Length >= x)|>ignore
        result

    let solve _ =
        firstFibHasDigit 1000

[<EntryPoint>]
let main argv =
    let (r, t) = Misc.Chrono.timeQuick Method1.solve ()
    printfn "%A Time used: %A ms" r t.TotalMilliseconds
    0 // return an integer exit code
