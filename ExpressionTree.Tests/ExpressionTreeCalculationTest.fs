module ExpressionTree.Tests

open NUnit.Framework
open FsUnit
open ExpressionTree

[<Test>]
let ``Calculation of Empty expression tree is correct`` () =
    let binTree = Empty
    calc binTree |> should (equalWithin 0.1) 0.0

[<Test>]
let ``Calculation of expression tree with only one value is correct`` () =
    let binTree = // (2 + 2) * 2
        Node(
            Multiplication,
            Node(Sum, Operand(2.0), Operand(2.0)),
            Operand(2.0)
        )

    calc binTree |> should (equalWithin 0.1) 8.0

[<Test>]
let ``Calculation of complex expression`` () =
    let binTree = // 25 * 7 + 17 / 9 / 3
        Node(
            Sum,
            Node(Multiplication, Operand(25.0), Operand(7.0)),
            Node(Division, Node(Division, Operand(17.0), Operand(9.0)), Operand(3.0))
        )

    calc binTree |> should (equalWithin 0.1) 175.6

[<Test>]
let ``Throws exception if expression is incorrect`` () =
    let binTree = Node(Multiplication, Empty, Empty)
    (fun () -> (calc binTree) |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Throws exception if there is devision by zero`` () =
    let binTree = Node(Division, Operand(2.0), Operand(0.0))

    (fun () -> (calc binTree) |> ignore) |> should throw typeof<System.Exception>
