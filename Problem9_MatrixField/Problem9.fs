//https://projecteuler.net/problem=9

(*
 *A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 *a^2 + b^2 = c^2
 *For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 *There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 *Find the product abc.
*)

module Method1 =
    let SUM = 1000I


///using Euclid's formula
module Method2 = 
    let SUM = Method1.SUM

    (*
     *Euclid's formula:
     *let m,n be two positive intergers and m>n
     *then (a,b,c) are Pythagorean triplet if
     *a=m**2-n**2, b=2*m*n, c=m**2+n**2
     *From a+b+c = SUM, we can derive: 2*m*(m+n)= SUM
     *Therefore, m*(m+n) = SUM/2
    *)

    let factorListOf x =
        let row = Math.Integer.factorizeBigint x
        let rec loop1 result = function
            |(_, 0) -> result
            |(bas, exp) -> loop1 (bas::result) (bas, exp-1)
        let rec loop2 result = function
            |[] -> result
            |head::tail -> loop2 (loop1 result head) tail
        loop2 [] row

    let solve sum =
        let factors = factorListOf (sum/2I)
        //this loop facorize (Sum/2I) into m*(m+n) where m>n
        let rec loop = function
            |x when x=factors.Length -> (0I, 0I)
            |x ->
                let (left, right) = List.splitAt x factors
                let m = List.fold (*) 1I left      // m = left
                let n = List.fold (*) 1I right - m // m+n=right -> n = right-m
                if m>n && n>bigint.Zero then (m,n) else loop (x+1)
        let (m,n) = loop 1
        let a=m**2 - n**2
        let b=2I*m*n
        let c=m**2+n**2
        a*b*c

[<EntryPoint>]
let main argv = 
    printfn "%A" (Method2.solve Method2.SUM)
    0 // return an integer exit code
