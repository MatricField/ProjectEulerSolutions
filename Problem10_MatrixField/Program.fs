// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Method1 =
    
    let target = 2000000

    let task x =
        Math.Integer.Prime.findPrimesBelowInt x
        |>List.map (fun x-> bigint(x))
        |>List.sum

    let solve =
        lazy (task target)

[<EntryPoint>]
let main argv = 
    printfn "%A" Method1.solve.Value
    0 // return an integer exit code
