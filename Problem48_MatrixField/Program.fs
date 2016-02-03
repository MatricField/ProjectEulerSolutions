//https://projecteuler.net/problem=48

(*
 *The series, 11 + 22 + 33 + ... + 1010 = 10405071317.
 *
 *Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*)

module Method1 =
    let value =
        lazy(
        [|1..1000|]
        |>Array.map (fun x -> (bigint x, x))
        |>Array.Parallel.map (fun (f,e) -> f**e)
        |>Array.sum
        |>string               //toString
        |>Seq.rev              //IEnumerable
        |>Seq.truncate 10
        |>Seq.rev
        |>Seq.toArray
        |>System.String
        |>System.Numerics.BigInteger.Parse
        )

[<EntryPoint>]
let main argv = 
    printfn "%A" Method1.value.Value
    0 // return an integer exit code
