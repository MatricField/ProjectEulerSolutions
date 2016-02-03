//https://projecteuler.net/problem=14

(*
 *The following iterative sequence is defined for the set of positive integers:
 *
 *n → n/2 (n is even)
 *n → 3n + 1 (n is odd)
 *
 *Using the rule above and starting with 13, we generate the following sequence:
 *
 *13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 *
 *Which starting number, under one million, produces the longest chain?
 *
 *NOTE: Once the chain starts the terms are allowed to go above one million.
*)

module Problem14

module Method1 =
    let collatzTranform = function
        |1L -> 0L //use zero as a sentinal
        |n when n%2L=0L -> n/2L
        |n -> 3L*n+1L

    let collatzSeq x =
        let rec loop result = function
            |0L -> result
            |n -> loop (n::result) (collatzTranform n)
        loop [] x

    let collatzSeqLen =
        let rec loop result = function
            |0L -> result
            |n -> loop (result+1L) (collatzTranform n)
        function
        |x when x>0L -> loop 0L x
        |_ -> raise (System.ArgumentException "Collatz transformation of non-positive number is undefined")
    

    let maxLen x =
        let rec loop maxTerm maxCount = function
            |1L -> (maxTerm,maxCount)
            |n when n>1L -> 
                match collatzSeqLen n with
                |count when count>maxCount -> loop n count (n-1L)
                |count -> loop maxTerm maxCount (n-1L)
            |_ -> failwith "x<=1"
        loop 1L 1L (x-1L)

module Method2 =
    let collatzTranform = Method1.collatzTranform

    ///function that generates a sequence of (startNumber, length) pair in range [1,x)
    let collatzSeqLens = 
        ///Generate a list of canidates and store them into the foundMap       
        let rec loop1 foundLst (foundMap:System.Collections.Hashtable) n = 
            let addToMap init =
                foundLst |>List.fold (fun maxCount nextElm -> foundMap.Add(nextElm, maxCount+1L); (maxCount+1L)) init
            match n with
            |x when foundMap.Contains x ->
                addToMap (foundMap.[x]:?>int64)
            |x -> loop1 (x::foundLst) foundMap (collatzTranform x)
        ///test each interger in [1,x)
        let rec loop2 (map:System.Collections.Hashtable) = function
            |0L -> map
            |x ->
                match x with
                |_ when map.ContainsKey x -> loop2 map (x-1L)
                |_->
                    loop1 [] map x |> ignore
                    loop2 map (x-1L)
        function
        |x ->
            let map = new System.Collections.Hashtable((int)x*3, (float32)1)
            map.Add(0L,0L)
            let collection = loop2 (map) x
            [|
                for i in collection do
                    let temp = i:?>System.Collections.DictionaryEntry
                    let term = temp.Key:?>int64
                    let count = temp.Value:?>int64
                    yield (term, count)
            |]

    let maxLen x =
        collatzSeqLens x
        |>Array.maxBy (fun (term,count) -> count)

let main argv =
    let (r1,t1) = Misc.Chrono.timeQuick Method1.maxLen 1000000L
    printfn "%A, %Ams" r1 t1.TotalMilliseconds

//    let (r2,t2) = Misc.Chrono.timeQuick Method2.maxLen 1000000L
//    printfn "%A, %Ams" r2 t2.TotalMilliseconds

    0 // return an integer exit code