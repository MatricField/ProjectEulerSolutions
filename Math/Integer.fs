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
                    |_ when (isPrime n primes) -> (*printfn "prime:%A" n;*) loop (n::primes) (n+1I)
                    |_ -> loop primes (n+1I)
                |_ -> raise (System.InvalidOperationException "Impossible state reached")
            loop [] 2I

        let findPrimesBelowInt x =
            let rec isPrime x = function
                |[] -> true
                |head::otherPrimes when (x % head <> 0) -> isPrime x otherPrimes
                |_ -> false

            let rec loop primes = function
                |n when n = x -> primes
                |n when n < x ->
                    match n with
                    |_ when (isPrime n primes) -> (*printfn "prime:%A" n;*) loop (n::primes) (n+1)
                    |_ -> loop primes (n+1)
                |_ -> raise (System.InvalidOperationException "Impossible state reached")
            loop [] 2

    ///function that factorize an integer and return a list of (factor, power) pairs
    let factorize =
        let findNextFactorOf n =
            let rec loop = function
                |k when (n%k = 0I) -> k
                |k -> loop (k+1I)
        
            function
            |[]-> loop 2I
            |(head,_)::tail -> loop head
    
        let fectorOut factor x =
            let rec loop count = function
            |remains when (remains=1I) || (remains%factor <> 0I) -> (remains, count)
            |n -> loop (count+1) (n/factor)
            loop 0 x
    
        let rec loop factorList n =
            let k = findNextFactorOf n factorList
            let (r,c) = fectorOut k n
            //printfn "prime factor: %A, power: %A, remains: %A" k c r
            match r with
                |next when next = 1I -> (k,c)::factorList
                |next ->loop ((k,c)::factorList) next
        function
        |n when n = 1I -> [(1I,1)]
        |n when n > 1I -> loop [] n
        |_ -> raise (System.NotImplementedException "factorization for negtive values are not supported")

