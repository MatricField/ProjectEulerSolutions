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

    let rec isPrime x = function
        |[] -> true
        |head::otherPrimes when (x % head <> 0I) -> isPrime x otherPrimes
        |_ -> false

    let findPrimeFactorsOf x =
        let rec loop primes primeFactors = function
            |n when n >= x/2I -> primeFactors
            |n when n < x/2I ->
                match n with
                |_ when (isPrime n primes) ->
                    match n with
                    |_ when (x%n = 0I) -> printfn"prime factor:%A" n; loop (n::primes) (n::primeFactors) (n+1I)
                    |_ -> loop (n::primes) primeFactors (n+1I)
                |_ -> loop primes primeFactors (n+1I)
            |_ -> raise (System.InvalidOperationException "Logic Error")

        loop [] [] 2I

    let solve x = findPrimeFactorsOf x |> List.head

//let rec sortedListEquals = function
//    |([],[]) -> true
//    |([],_) -> false
//    |(_,[]) -> false
//    |(head1::tail1, head2::tail2) when (head1 = head2) -> sortedListEquals (tail1,tail2)
//    |(head1::tail1, head2::tail2) when (head1 <> head2) -> false

[<EntryPoint>]
let main argv =
    Method3.solve TARGET
    |>printfn "%A"
    0 // return an integer exit code
