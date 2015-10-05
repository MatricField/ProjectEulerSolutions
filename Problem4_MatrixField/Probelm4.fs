//https://projecteuler.net/problem=4

(*
 *A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 *Find the largest palindrome made from the product of two 3-digit numbers.
*)

module Method1 =
    
    let factorize = Probelm3.Method4.factorize

    // failed method
    let lowerBound = 10I*10I

    let upperBound = 99I*99I

    let digitsOf num =
        let rec loop result = function
        |n when n = 0I -> result
        |n -> loop (n%10I::result) (n/10I)
        loop [] num

    let checkIfPal arrOfDigits =
        let reversed = arrOfDigits|> List.rev
        let rec equal = function
            |(head1::tail1, head2::tail2) when head1 <> head2 -> false
            |(head1::tail1, head2::tail2) -> equal (tail1, tail2)
            |([],[])->true
            |_ -> raise (System.InvalidOperationException "")
        equal (arrOfDigits, reversed)

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
            |[factor] when factor = head -> findComposite result tail
            |_ -> findComposite (head::result) tail

    let solve =
        scan [] (fun x-> x-1I) upperBound
        |>findComposite []

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
    
    let solve =
        scan (fun x-> x-1I) [] (MAX,MAX)
        |>List.map (fun x-> printfn "%A" x; x)
        |>List.toArray
        |>Array.sortDescending
        |>Array.head
[<EntryPoint>]
let main argv = 
    Method2.solve
    //|>List.head
    |>printfn "Solution: %A"
    0 // return an integer exit code
