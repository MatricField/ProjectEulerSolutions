//https://projecteuler.net/problem=3

(*
 *The prime factors of 13195 are 5, 7, 13 and 29.
 *What is the largest prime factor of the number 600851475143?
 *)

let TARGET = 600851475143I

module Method1 =
    let findPrimesBelow x =
        let candidates = [2I..x]

        let search lst primeFactor =
            [for n in lst do
                if n % primeFactor <> 0I then yield n]

        let rec loop primes = function
            | [] -> primes
            | head :: others -> loop (head :: primes) (search others head)
        loop [] candidates

    let findPrimeFactorsOf x =
        let candidates = findPrimesBelow (x/2I)

        let rec search found = function
            |[] -> found
            |n :: rest when (x%n = 0I) -> search (n::found) rest
            |n :: rest -> search found rest

        search [] candidates

//    let solve =
//        findPrimeFactorsOf TARGET
//        |>List.tail

module Method2 =
    // search primes in-place
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

    let findPrimeFactorsOf x =
        let candidates = findPrimesBelow (x/2I)

        let rec search found = function
            |[] -> found
            |n :: rest when (x%n = 0I) -> printfn "prime factor of %A: %A" x n; search (n::found) rest
            |n :: rest -> search found rest

        search [] candidates

    let solve x =
        findPrimeFactorsOf x
        |>List.tail

module Method3 =
    // search primes factors in-place

    let productOf lst =
        let rec loop result = function
            |[] -> result
            |head::tail -> loop (head*result) tail
        loop 1I lst

    //given a list of known primes, use flitering method to determines if x is a prime
    let rec isPrime x = function
        |head::otherPrimes when (x % head <> 0I) -> isPrime x otherPrimes
        |[] -> true
        |_ -> false

    let findPrimeFactorsOf x =
        let rec loop primes primeFactors = function
            |n when n >= x/2I -> primeFactors
            |n when n < x/2I ->
                match n with
                |_ when (isPrime n primes) ->
                    match n with
                    |_ when (x%n = 0I) ->
                        printfn"prime factor:%A" n
                        let futureFactorList = primeFactors |> List.append [n]
                        match productOf futureFactorList with
                        |m when m> x/2I -> futureFactorList
                        |_ -> loop (n::primes) futureFactorList (n+1I)
                    |_ -> loop (n::primes) primeFactors (n+1I)
                |_ -> loop primes primeFactors (n+1I)
            |_ -> raise (System.InvalidOperationException "Logic Error")

        loop [] [] 2I

    let solve =
     findPrimeFactorsOf TARGET |> List.head

module Method4 =
    //using unique factorization theorm
    let factorize x =
        let findNextFactorOf n x =

            let rec loop = function
                |k when (x%k = 0I) -> k
                |k -> loop (k+1I)

            loop n

        let fectorOut factor x =
            let rec loop count = function
            |remains when (remains=1I) || (remains%factor <> 0I) -> (remains, factor, count)
            |n -> loop (count+1I) (n/factor)
            loop 0I x

        let rec loop factorList n =
            let (r,k,c) = fectorOut (factorList|>List.head) n
            printfn "prime factor: %A, power: %A, remains: %A" k c r
            match r with
                |next when next = 1I -> factorList
                |next ->loop ((findNextFactorOf k next)::factorList) next

        loop [findNextFactorOf 2I x] x
    let solve =
        factorize TARGET |> List.head

[<EntryPoint>]
let main argv =
    Method4.solve
    |>printfn "%A"
    0 // return an integer exit code
