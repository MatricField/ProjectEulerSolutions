namespace Math
module Integer =
    module Prime =
        let findPrimesBelow x =
            let rec isPrime x = function
                |[] -> true
                |head::otherPrimes when (x % head <> 0I) -> isPrime x otherPrimes
                |_ -> false

            let rec loop primes = function
                |n when n = x -> primes
                |n when n < x ->
                    match n with
                    |_ when (isPrime n primes) -> printfn "prime:%A" n; loop (n::primes) (n+1I)
                    |_ -> loop primes (n+1I)
                |_ -> raise (System.InvalidOperationException "Impossible state reached")
            loop [] 2I

