//https://projecteuler.net/problem = 5

(*
 *2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)

module Method1 =
    let factorize = Probelm4.Method1.factorize

    let min = 1I
    let max = 20I

    let rawList =
        let rec loop result = function
            |n when n>max || n<min -> result
            |n -> loop ((factorize n)@result) (n+1I)
        loop [] min

    let lcm =
        rawList
        |>List.groupBy (fun (factor,_) -> factor)
        |>List.map (fun (_,lst) -> lst)
        |>List.map (List.sortByDescending (fun(_,power) ->power))
        |>List.map (fun lst -> lst.Head)
        |>List.sortByDescending (fun (factor,_) -> factor)

    let pow bas exp =

        let rec loop result = function
            |n when n = 0I -> result
            |n -> loop (result*bas) (n-1I)

        match exp with
        |_ when exp = 0I -> 1I
        |_ -> loop 1I exp

    let solve () =
        let rec loop result = function
            |[] -> result
            |(factor, power)::tail -> loop (result * (pow factor power)) tail
        loop 1I lcm

module Method2 =
    // using greatest common diviser

    let min = Method1.min
    let max = Method1.max
    //let max = 2I ** 64

    let rec gcd = function
        |(a,b) when b = 0I -> a
        |(a,b) -> gcd(b, a%b)

    let lcm = function
        |(a,b) when a<b -> a*(b/gcd(b,a))
        |(a,b) -> b*(a/gcd(a,b))

    let arrLcm arr =
        let rec loop = function
            |[|a;b|] -> lcm(a,b)
            |[|a|] -> a
            |[||] -> raise (System.ArgumentException "null array provided to calculate lcm")
            |arr ->
                arr
                |>Array.splitInto 2
                |>Array.Parallel.map loop
                |>loop
        loop arr

    let lstLcm lst =
        let rec loop result = function
            |head::tail -> loop (lcm(result,head)) tail
            |[]->result
        loop 1I lst

    let solve () = arrLcm [|min..max|]

let time func vars =
    let t1 = System.DateTime.Now

    let result = func vars

    let t2 = System.DateTime.Now

    (result, t2-t1)

let writeLine path content = System.IO.File.AppendAllText(path,content)

[<EntryPoint>]
let main argv =
    //printfn "%A" (Method1.solve ())

//    let (a,t) = time (Method2.lcm) (12I,18I)
//
//    printfn "%A %Ams" a t.TotalMilliseconds

//    let (a,t) = time (Method2.solve) ()
//
//    printfn "%A %Ams" a t.TotalMilliseconds

    for i in [1..128] do
        let arr = [|1I.. 2I**i|]

        let(_, tarr) = time (Method2.arrLcm) arr

        let lst = arr|>Array.toList

        let(_, tlst) = time (Method2.lstLcm) lst

        printfn "arr:%A ms  lst:%A ms" tarr.TotalMilliseconds tlst.TotalMilliseconds

        let output = "i=" + (string)arr.LongLength + "\t" + (string)tarr.TotalMilliseconds + "\t" + (string)tlst.TotalMilliseconds + "\n"

        writeLine "Log.txt" output

    0 // return an integer exit code
