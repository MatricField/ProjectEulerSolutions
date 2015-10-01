//https://projecteuler.net/problem=1

(*
*If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
*
*Find the sum of all the multiples of 3 or 5 below 1000.
*)

//**********************Solution**************************

let MAX = 1000 //Constant to store the max

//*************************************************************************************************

// Method 1:
// naive search

module Method1 =
    begin

    //Function to fetch a list of all multiples of "value" ranges from [0, upperLimit)
    let searchMultiplesOf value upperLimit = 
        let rec search = function
            |m when m*value < upperLimit -> m*value :: search (m + 1)
            |m when m*value >= upperLimit -> []
            |_ -> []
        search 1
open System.Linq
    let solve = 
        let list1 = searchMultiplesOf 3 MAX
        let list2 = searchMultiplesOf 5 MAX
        let list3 = list1.Intersect list2

        let outList list =
            list|>List.map (fun x-> printfn "%A\n" x)

        //outList list1
        //outList list2
        //list3.ToList().ForEach (fun x -> printfn "%A\n" x)

        (list1.Sum(fun x->x) + list2.Sum(fun x->x) - list3.Sum(fun x->x))
    end
    

[<EntryPoint>]
let main argv =
    printfn "%A\n" Method1.solve

    0 // return an integer exit code
