namespace Misc

module Text =
    module ListParser =
        ///seprate each entry with white space characters
        let breakWhiteSpace str =
            System.Text.RegularExpressions.Regex.Replace(str,"\s+","|")
            |> fun x-> x.Split('|')

        ///seprate each entry with newLine characters
        let breakNewLines (str:string) =
            str.Split('\n')
            |>Array.Parallel.map(fun str -> System.String.Intern(str).Trim())

    module TableParser =
        ///use newLine to seperate rows and other white space to seperate columns
        let parse str =
            System.Text.RegularExpressions.Regex.Replace(str,"\n+","|")
            |>fun x -> x.Split('|')
            |>Array.Parallel.map (fun str -> str.Trim())
            |>Array.Parallel.map (fun str -> System.Text.RegularExpressions.Regex.Replace(str,"\s+","|"))
            |>Array.Parallel.map (fun str -> str.Split('|'))

    let printTable table =
        for line in table do
            for elm in line do
                printf "%A " elm
            printfn ""