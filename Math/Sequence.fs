namespace Math

module Sequence =
    ///Packaged functions for Lucas Sequences
    module Lucas =
        /// implements the recursive definition of Lucas Sequences:
        /// Xn = P*Xn-1 - Q* Xn-2
        let inline next prev1 prev2 P Q = P*prev1 - Q*prev2

        /// implements the computation of a Lucas Sequence
        /// condition is a function that determines when to stop
        /// where the first parameter is the index of the current Lucas number
        /// the second is the current Lucas number
        let mainLoop A0 A1 P Q condition=
            let inline advance prev1 prev2 = next prev1 prev2 P Q
            let rec loop prev1 prev2 = function
                |count when condition count prev1 -> prev1
                |count -> loop (advance prev1 prev2) prev1 (count+1I)
            loop A1 A0 1I

        let compute A0 A1 P Q = function
            |x when x<0I -> raise (System.ArgumentException "lucas sequence of x when x<0 is undefined")
            |x when x=0I -> A0
            |x -> mainLoop A0 A1 P Q (fun n _ ->n<x)

        /// implements the recursive definition of Lucas Sequences:
        /// Xn = P*Xn-1 - Q* Xn-2, returns a list of result
        let generate A0 A1 P Q x =
            let rec loop result prev1 prev2 = function
                |count when count = x -> result
                |count ->
                    let current = (P*prev1 - Q*prev2)
                    loop (prev1::result) current prev1 (count+1I)
            match x with
            |_ when x<0I -> raise (System.ArgumentException "lucas sequence of x when x<0 is undefined")
            |_ when x=0I -> [A0]
            |_ -> loop [] A1 A0 1I

        /// the U form of Lucas Sequences numbers:
        /// U(0)=0I, U(1)=1I
        let inline U P Q = compute 0I 1I P Q

        ///generate a Lucas Sequences in U form:
        let inline Ulst P Q = generate 0I 1I P Q
        
        /// the V form of Lucas Sequences numbers:
        /// V(0)=2I, V(1)=P
        let inline V P Q = compute 2I P P Q

        /// generate a Lucas Sequences in V form:
        /// V(0)=2I, V(1)=P
        let inline Vlst P Q = generate 2I P P Q

        /// find the Fibbocino number fib(x)
        let inline fib x = U 1I -1I x

        /// generate a Fibbocino sequence for agument in range [0..x)
        let inline fibLst x = Ulst 1I -1I x

        /// find the Lucas number lucas(x)
        let inline lucas x = V 1I -1I x

        let inline pell x = U 2I -1I x