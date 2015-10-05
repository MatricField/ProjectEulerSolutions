//https://projecteuler.net/problem = 5

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

    let solve =
        let rec loop result = function
            |[] -> result
            |(factor, power)::tail -> loop (result * (pow factor power)) tail
        loop 1I lcm

[<EntryPoint>]
let main argv =
    printfn "%A" Method1.solve
    0 // return an integer exit code
