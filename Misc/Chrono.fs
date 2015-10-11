namespace Misc

module Chrono =

    let time func vars =
        let t1 = System.DateTime.Now

        let result = func vars

        let t2 = System.DateTime.Now

        (result, t2-t1)
