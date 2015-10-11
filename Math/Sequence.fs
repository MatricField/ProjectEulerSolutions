namespace Math

module Sequence =
    module Lucas =
        let compute A0 A1 P Q x =
            let rec loop prev1 prev2 = function
                |count when count = x -> prev1
                |count -> loop (P*prev1 - Q*prev2) prev1 (count+1)

            match x with
            |_ when x<0 -> raise (System.ArgumentException "lucas sequence of x when x<0 is undefined")
            |0 -> A0
            |_ -> loop A1 A0 1

        let U P Q = compute 0I 1I P Q
    
        let V P Q = compute 2I P P Q

        let fib = U 1I -1I

        let lucas = V 1I -1I

        let pell = U 2I -1I