namespace Misc

module DebugTools =
    type TestResult =
        |Passed
        |NotPassed
    let check_expect func argc expectedResult=
        if (func argc = expectedResult) then Passed else NotPassed

    let test func argc expectedResult =
        let res = func argc
        if (res = expectedResult) then
            printfn "%s" "test passed"
        else
            printfn "test failed when call function %A with %A. %A is expected while %A is returned" func argc expectedResult res