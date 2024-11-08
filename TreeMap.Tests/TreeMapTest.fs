module TreeMap.Tests

open NUnit.Framework
open FsUnit
open TreeMapFunc

[<Test>]
let ``Equality function works correctly`` () =
      let binTree1 = Empty
      let binTree2 = Empty
      let binTree3 = Node(1, Empty, Empty)

      (equalityTreeChecker binTree1 binTree2, equalityTreeChecker binTree1 binTree3) |> should equal (true, false)

[<Test>]
let ``Map function works correctly`` () =
      let binTree = Node(1,
                        Node(2, Empty, Empty),
                            Node(3, Empty, Empty))
      let correctResTree = Node(2,
                              Node(4, Empty, Empty),
                                  Node(6, Empty, Empty))
      let resTree = map (fun x -> 2 * x) binTree

      equalityTreeChecker correctResTree resTree |> should be True
