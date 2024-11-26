module Test.Tests

open NUnit.Framework
open FsUnit
open Test

//find min tests
[<Test>]
let ``If ls is empty returns None test`` () = findMinElement [] |> should equal None

[<Test>]
let ``Works correctly on int`` () =
    findMinElement [ 1; 2; 3; -19; 0 ] |> should equal <| Some(-19)

[<Test>]
let ``Works correctly on string`` () =
    findMinElement [ "5"; "3"; "" ] |> should equal <| Some("")

//tree tests
[<Test>]
let ``If tree is Empty returns 0 test`` () =
    let tree = Empty
    findMinDistance tree |> should equal <| None

[<Test>]
let ``Returns correct distance test`` () =
    let tree = Node(2, Empty, Empty)
    findMinDistance tree |> should equal <| Some(0)

[<Test>]
let ``Returns correct distance in complex tree test`` () =
    let tree =
        Node(3, Node(3, Node(3, Empty, Empty), Node(3, Empty, Empty)), Node(3, Empty, Empty))

    findMinDistance tree |> should equal <| Some(1)

//hash-table tests
let hashTable = HashTable(5, id)

[<Test>]
let ``Adding more than one test`` () =
    hashTable.Add(5)
    hashTable.Add(10)
    hashTable.Contains(5) |> should be True
    hashTable.Contains(10) |> should be True

[<Test>]
let ``Remove test`` () =
    hashTable.Remove(5) |> should be True
    hashTable.Contains(5) |> should be False
