//https://projecteuler.net/problem=4

(*
 *A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 *Find the largest palindrome made from the product of two 3-digit numbers.
*)

module Method1 =
    
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
            |n -> loop (count+1I) (n/factor)
            loop 0I x
    
        let rec loop factorList n =
            let k = findNextFactorOf n factorList
            let (r,c) = fectorOut k n
            //printfn "prime factor: %A, power: %A, remains: %A" k c r
            match r with
                |next when next = 1I -> (k,c)::factorList
                |next ->loop ((k,c)::factorList) next
        function
        |n when n = 1I -> [(1I,1I)]
        |n when n > 1I -> loop [] n
        |_ -> raise (System.NotImplementedException "factorization for negtive values are not supported")

    // failed method
    let lowerBound = 10I*10I

    let upperBound = 99I*99I

    let digitsOf num =
        let rec loop result = function
        |n when n = 0I -> result
        |n -> loop (n%10I::result) (n/10I)
        loop [] num

    let checkIfPal arrOfDigits =
        (arrOfDigits = (arrOfDigits|>List.rev))

    let isPal num =
        digitsOf num |> checkIfPal

    let rec scan result iter = function
        |n when isPal n -> printfn "%A" n; scan (n::result) iter (iter n)
        |n when n<lowerBound || n>upperBound -> result
        |n -> scan result iter (iter n)

    let rec findComposite result = function
        |[] -> result
        |head::tail ->
            match factorize head with
            |[(factor,_)] when factor = head -> findComposite result tail
            |_ -> findComposite (head::result) tail

//    let solve =
//        scan [] (fun x-> x-1I) upperBound
//        |>findComposite []

module Method2 =
    let isPal = Method1.isPal

    let MIN = 100I
    let MAX = 999I

    let outOfBound x = x<MIN || x>MAX

    let rec scan iter result = function
        |(a,b) when outOfBound b -> result
        |(a,b) when outOfBound a -> scan iter result (iter b, iter b)
        |(a,b) ->
            match a*b with
            |x when isPal x -> scan iter (x::result) (iter a, b)
            |_ -> scan iter result (iter a, b)
    
    let solve () =
        scan (fun x-> x-1I) [] (MAX,MAX)
        //|>List.map (fun x-> printfn "%A" x; x)
        |>List.toArray
        |>Array.sortDescending
        |>Array.head



[<EntryPoint>]
let main argv = 
    Method2.solve()
    |>printfn "Solution: %A"

    0 // return an integer exit code
