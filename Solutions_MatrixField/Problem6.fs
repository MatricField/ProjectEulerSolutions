//https://projecteuler.net/problem=6

(*
 *The sum of the squares of the first ten natural numbers is,
 *
 *12 + 22 + ... + 102 = 385
 *The square of the sum of the first ten natural numbers is,
 *
 *(1 + 2 + ... + 10)2 = 552 = 3025
 *Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
 *
 *Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 *
*)

module Problem6

module Method1 =
    let steps x = x+1I

    let sumSquare init steps inRange =
        let rec loop result = function
            |n when not (inRange n) -> result
            |n -> loop (result + (n**2)) (steps n)
        loop 0I init

    let squareSum init steps inRange =
        let rec loop result = function
            |n when not (inRange n) -> result
            |n -> loop (result + n) (steps n)
        (loop 0I init)**2

    let less a b = b < a

    let greater a b = b > a

    let solve () =
        let init = 1I
        let final = 100I
        let steps = fun x -> x+1I
        let condition = less (steps final)
        (squareSum init steps condition) - (sumSquare init steps condition)

let main argv = 
    
    let (result,time) = Misc.Chrono.time Method1.solve ()
    printfn "%A %Ams" result time.TotalMilliseconds
    0 // return an integer exit code
