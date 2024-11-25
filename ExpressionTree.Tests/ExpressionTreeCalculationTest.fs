module ExpressionTree.Tests

open NUnit.Framework
open FsUnit
open ExpressionTree

[<Test>]
let ``Calculation of Empty expression tree is correct`` () =
    let binTree = Empty
    ((calc binTree - 0.0) <= differencePrecision) |> should be True

[<Test>]
let ``Calculation of expression tree wis only one value is correct`` () =
    let binTree = // (2 + 2) * 2
        Node(
            TreeOperator(Multiplication),
            Node(TreeOperator(Sum), Node(Operand(2.0), Empty, Empty), Node(Operand(2.0), Empty, Empty)),
            Node(Operand(2.0), Empty, Empty)
        )

    ((calc binTree - 8.0) <= differencePrecision) |> should be True

[<Test>]
let ``Calculation of complex expression`` () =
    let binTree = // 25 * 7 + 17 / 9 / 3
        Node(
            TreeOperator(Sum),
            Node(TreeOperator(Multiplication), Node(Operand(25.0), Empty, Empty), Node(Operand(7.0), Empty, Empty)),
            Node(
                TreeOperator(Division),
                Node(TreeOperator(Division), Node(Operand(17.0), Empty, Empty), Node(Operand(9.0), Empty, Empty)),
                Node(Operand(3.0), Empty, Empty)
            )
        )

    ((calc binTree - 175.6) <= differencePrecision) |> should be True

[<Test>]
let ``Throws exception if expression is incorrect`` () =
    let binTree = Node(TreeOperator(Multiplication), Empty, Empty)
    (fun () -> (calc binTree) |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Throws exception if there is devision by zero`` () =
    let binTree =
        Node(TreeOperator(Division), Node(Operand(2.0), Empty, Empty), Node(Operand(0.0), Empty, Empty))

    (fun () -> (calc binTree) |> ignore) |> should throw typeof<System.Exception>
