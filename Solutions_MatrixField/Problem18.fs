//https://projecteuler.net/problem=18

(*
 *By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
 *
 *3
 *7 4
 *2 4 6
 *8 5 9 3
 *
 *That is, 3 + 7 + 4 + 9 = 23.
 *
 *Find the maximum total from top to bottom of the triangle below:
 *
 *75
 *95 64
 *17 47 82
 *18 35 87 10
 *20 04 82 47 65
 *19 01 23 75 03 34
 *88 02 77 73 07 63 67
 *99 65 04 28 06 16 70 92
 *41 41 26 56 83 40 80 70 33
 *41 48 72 33 47 32 37 16 94 29
 *53 71 44 65 25 43 91 52 97 51 14
 *70 11 33 28 77 73 17 78 39 68 17 57
 *91 71 52 38 17 14 91 43 58 50 27 29 48
 *63 66 04 68 89 53 67 30 73 16 69 87 40 31
 *04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
*)

module Problem18

open System.Collections.Generic
let text =
    "75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

let text1 =
    "3
    7 4
    2 4 6
    8 5 9 3"

module NotWorking =

    let makeRecord paredTable =
            let createLinkedList seq =
                let result = LinkedList<int64>()
                for elm in seq do
                    result.AddLast(System.Int64.Parse(elm))|>ignore
                result
            paredTable
            |>Array.map (fun line -> createLinkedList line)
            |>Array.toList

    let maxOf a b c =
        if a > c then
            if b > a then b else a
        else
            if b > c then b else c

    let rec maxPathSum = function
        |(head:LinkedList<int64>)::tail->
            match tail with
            |mid::rest ->
                let next =
                    let left = mid.First.Value
                    let right = mid.Last.Value
                    if left > right then left else right
                let left,right =
                    let mutable left=0L
                    let mutable right=0L
                    for (line:LinkedList<int64>) in tail do
                        left<-left+line.First.Value
                        right<-right+line.Last.Value
                        line.RemoveFirst();line.RemoveLast()
                    left,right
                let sub = next + (maxPathSum rest)
                head.First.Value + (maxOf left right sub)
            |[] -> head.First.Value
        |[] -> 0L

    let rec maxPathSum2 = function
        |(head:LinkedList<int64>)::tail->
            let left = [for line in tail do yield line.First.Value]|>List.sum
            let right = [for line in tail do yield line.Last.Value]|>List.sum
            if left<right then
                for line in tail do
                    line.RemoveFirst()
            else
                for line in tail do
                    line.RemoveLast()
            head.First.Value + (maxPathSum2 tail)
        |[]->0L

module Method1 =
    /// A function that read the triangle in to an Array of Arrays of string entry
    let parse = Misc.Text.TableParser.parse
    /// convert a string[][] into a int64[][]
    let makeHeap parsedTable =
            parsedTable
            |>Array.map (Array.map System.Int64.Parse)

    /// function to find local maximum
    /// yield an array containing the local max whose root are nodes in the top
    let combine top bottem =
            [|
            for i in 0..(Array.length top - 1) do
                let a = Array.get bottem i
                let b = Array.get bottem (i+1)
                let c = Array.get top i
                if a>b then
                    yield a+c
                else
                    yield b+c
            |]

    /// calculate the maximum path sum in a number triangle
    /// with O(n^2) time complexity
    let maxPathSumOf heap =
        heap
        |>Array.reduceBack combine // find the local max bottom-up
        |>Array.head // the local max of the root is the maximum

    /// helper that processes the text
    let readTriangle inputText =
        inputText
        |>parse
        |>makeHeap

    let solve inputText =
        inputText
        |>readTriangle
        |>maxPathSumOf

module Method2 =
    let readTriangle = Method1.readTriangle

    let maxPathSumOf heap=
        let rec maxSum heap row col =
            if Array.length heap - row < 1 then 0L,[]
            else if Array.length heap.[row] - col < 1 then
                raise (System.InvalidOperationException())
            else
                let n = heap.[row].[col]
                let a,alst = maxSum heap (row+1) (col)
                let b,blst = maxSum heap (row+1) (col+1)
                if a>b then a+n,n::alst else b+n,n::blst
        maxSum heap 0 0

    let solve inputText =
        inputText
        |>readTriangle
        |>maxPathSumOf

module Method1_1 = 
    let readTriangle = Method2.readTriangle
    //compact way to write the method
    let combine nextLine prevResult =
        prevResult
        |>Array.pairwise
        |>Array.map2 (fun x (sum1,sum2)-> if sum1>sum2 then x+sum1 else x+sum2) nextLine

    let maxPathSumOf heap =
        Array.reduceBack combine heap |> Seq.head

    let solve inputText =
        inputText
        |>readTriangle
        |>maxPathSumOf

let main argv =
    //let r,t = Misc.Chrono.timeQuick Method1.solve text
    let r,t = Misc.Chrono.timeQuick Method1.solve text1
    printfn "The sum is %d, time used: %Ams" r t.TotalMilliseconds
    let r1,t1 = Misc.Chrono.timeQuick Method1_1.solve text1
    printfn "The sum is %d, time used: %Ams" r1 t1.TotalMilliseconds
    0 // return an integer exit code

