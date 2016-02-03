namespace Math
module Integer =

    let sqrtBigint x =
        float x
        |>sqrt
        |>System.Math.Floor
        |>string
        |>System.Numerics.BigInteger.Parse

    let sqrtInt x =
        float x
        |>sqrt
        |>System.Math.Floor
        |>int

    module Prime =

        let findPrimesBelowBigint x =
            let rec loop result = function
                |head::tail when head <= sqrtBigint x -> loop <| head::result <| List.filter (fun x-> x%head<>0I) tail
                |lst -> result@lst
                |[] -> result
            loop [] [2I..x-1I]

        let findPrimesBelow x =
            let rec loop result = function
                |head::tail when head <= sqrtInt x -> loop <| head::result <| List.filter (fun x-> x%head<>0) tail
                |lst -> result@lst
                |[] -> result
            loop [] [2..x-1]

    ///function that factorize an integer and return a list of (factor, power) pairs
    let factorizeBigint =
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
        |_ -> raise <| System.NotImplementedException "factorization for negtive values are not supported"

    let factorizeL =
        let findNextFactorOf n =
            let rec loop = function
                |k when (n%k = 0L) -> k
                |k -> loop (k+1L)
        
            function
            |[]-> loop 2L
            |(head,_)::tail -> loop head
    
        let fectorOut factor x =
            let rec loop count = function
            |remains when (remains=1L) || (remains%factor <> 0L) -> (remains, count)
            |n -> loop (count+1) (n/factor)
            loop 0 x
    
        let rec loop factorList n =
            let k = findNextFactorOf n factorList
            let (r,c) = fectorOut k n
            //printfn "prime factor: %A, power: %A, remains: %A" k c r
            match r with
                |next when next = 1L -> (k,c)::factorList
                |next ->loop ((k,c)::factorList) next
        function
        |1L -> [(1L,1)]
        |n when n > 1L -> loop [] n
        |_ -> raise (System.NotImplementedException "factorization for negtive values are not supported")

    let factorize =
        let findNextFactorOf n =
            let rec loop = function
                |k when (n%k = 0) -> k
                |k -> loop (k+1)
        
            function
            |[]-> loop 2
            |(head,_)::tail -> loop head
    
        let fectorOut factor x =
            let rec loop count = function
            |remains when (remains=1) || (remains%factor <> 0) -> (remains, count)
            |n -> loop (count+1) (n/factor)
            loop 0 x
    
        let rec loop factorList n =
            let k = findNextFactorOf n factorList
            let (r,c) = fectorOut k n
            //printfn "prime factor: %A, power: %A, remains: %A" k c r
            match r with
                |next when next = 1 -> (k,c)::factorList
                |next ->loop ((k,c)::factorList) next
        function
        |1 -> [(1,1)]
        |n when n > 1 -> loop [] n
        |_ -> raise (System.NotImplementedException "factorization for negtive values are not supported")

    let nonPrimeFactorize =
        let findNextFactorOf n =
            let rec loop = function
                |k when (n%k = 0) -> k
                |k -> loop (k+1)
        
            function
            |[]-> loop 2
            |head::tail -> loop head
    
        let fectorOut factor x =
            let rec loop result = function
            |remains when (remains=1) || (remains%factor <> 0) -> remains::result
            |remains -> loop (remains::result) (remains/factor)
            loop [] (x/factor)
    
        let rec loop factorList n =
            let k = findNextFactorOf n factorList
            let factors = fectorOut k n
            //printfn "prime factor: %A, power: %A, remains: %A" k c r
            match factors with
                |_ when factors.Head = 1 -> factors@factorList
                |_ ->loop (factors@factorList) factors.Head
        function
        |1 -> [1]
        |n when n > 1 -> loop [] n
        |_ -> raise (System.NotImplementedException "factorization for negtive values are not supported")
