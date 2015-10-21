namespace Misc

module Chrono =

    let time func vars =
        func vars |> ignore //get rid of the compile time

        let t1 = System.DateTime.Now

        let result = func vars

        let t2 = System.DateTime.Now

        (result, t2-t1)
