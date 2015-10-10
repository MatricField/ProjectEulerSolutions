// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let time func vars =
    let t1 = System.DateTime.Now

    let result = func vars

    let t2 = System.DateTime.Now

    (result, t2-t1)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
