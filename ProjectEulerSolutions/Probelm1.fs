//https://projecteuler.net/problem=1

(*
*If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
*
*Find the sum of all the multiples of 3 or 5 below 1000.
*)

//**********************Solution**************************

let MAX = 1000 //Constant to store the max

//************************************************************************************************


module Method1 = // naive search
    begin

    //Function to fetch a list of all multiples of "value" ranges from [0, upperLimit)
    let searchMultiplesOf value upperLimit = 
        let rec search = function
            |m when m*value < upperLimit -> m*value :: search (m + 1)
            |m when m*value >= upperLimit -> []
            |_ -> []
        search 1
open System.Linq
    let solve () = 
        let list1 = searchMultiplesOf 3 MAX
        let list2 = searchMultiplesOf 5 MAX
        let list3 = list1.Intersect list2

        //let outList list =
        //    list|>List.map (fun x-> printfn "%A\n" x)

        //outList list1
        //outList list2
        //list3.ToList().ForEach (fun x -> printfn "%A\n" x)

        (list1.Sum(fun x->x) + list2.Sum(fun x->x) - list3.Sum(fun x->x))
    end


module Method2 = 
   (*
    * suppose x is the target factor
    * Obtain multiples by generating a factor list in range [0,S) where S = MAX mod x
    * and then multiply them with x
    *)
    begin

//    let getSum x =
//        let a = (MAX - 1) / x // get the maximum factor S, using (MAX - 1) to make sure a < S: see below
//        [0..a]// list includes a, so a<S is required
//        |>List.map(fun i -> i*x)
//        //|>List.map(fun x -> printfn "%A\n" x; x)
//        |>List.sum

    let getSum x =
        let a = (MAX - 1)/x
        (0 + a*x) * (a + 1) / 2

    let solve () =
        getSum 3 + getSum 5 - getSum (3*5)

    end

[<EntryPoint>]
let main argv =
    printfn "%A\n" <| Method1.solve()
    printfn "%A\n" <| Method2.solve()
    0 // return an integer exit code
