namespace Misc

module Chrono =

    /// increase accuracy for quick functions by elimilating compile time
    let timeQuick func vars =
        func vars |> ignore //get rid of the compile time

        let t1 = System.DateTime.Now

        let result = func vars

        let t2 = System.DateTime.Now

        (result, t2-t1)

    /// returns a (result, timeSpan) pair
    let time func vars =

        let t1 = System.DateTime.Now

        let result = func vars

        let t2 = System.DateTime.Now

        (result, t2-t1)