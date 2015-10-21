namespace Math

module Sequence =
    ///Packaged functions for Lucas Sequences
    module Lucas =
        /// implements the recursive definition of Lucas Sequences:
        /// Xn = P*Xn-1 - Q* Xn-2
        let compute A0 A1 P Q x =
            let rec loop prev1 prev2 = function
                |count when count = x -> prev1
                |count -> loop (P*prev1 - Q*prev2) prev1 (count+1I)
            match x with
            |_ when x<0I -> raise (System.ArgumentException "lucas sequence of x when x<0 is undefined")
            |_ when x=0I -> A0
            |_ -> loop A1 A0 1I

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
        let U P Q = compute 0I 1I P Q

        ///generate a Lucas Sequences in U form:
        let Ulst P Q = generate 0I 1I P Q
        
        /// the V form of Lucas Sequences numbers:
        /// V(0)=2I, V(1)=P
        let V P Q = compute 2I P P Q

        /// generate a Lucas Sequences in V form:
        /// V(0)=2I, V(1)=P
        let Vlst P Q = generate 2I P P Q

        /// find the Fibbocino number fib(x)
        let fib = U 1I -1I

        /// generate a Fibbocino sequence for agument in range [0..x)
        let fibLst = Ulst 1I -1I

        /// find the Lucas number fib(x)
        let lucas = V 1I -1I

        let pell = U 2I -1I