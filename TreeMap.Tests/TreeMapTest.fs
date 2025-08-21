module TreeMap.Tests

open NUnit.Framework
open FsUnit
open TreeMapFunc

[<Test>]
let ``Map function works correctly on int`` () =
    let binTree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let correctResTree = Node(2, Node(4, Empty, Empty), Node(6, Empty, Empty))
    let resTree = map (fun x -> 2 * x) binTree

    resTree |> should equal correctResTree

[<Test>]
let ``Map function works correctly on string`` () =
    let binTree = Node("1", Node("2", Empty, Empty), Node("3", Empty, Empty))
    let correctResTree = Node("11", Node("22", Empty, Empty), Node("33", Empty, Empty))
    let resTree = map (fun x -> x + x) binTree

    resTree |> should equal correctResTree

[<Test>]
let ``Map function works correctly on Empty`` () =
    let resTree = map id Empty

    resTree |> should equal Empty
