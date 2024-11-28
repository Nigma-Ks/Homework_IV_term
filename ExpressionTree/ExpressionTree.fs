namespace ExpressionTree

open System

module ExpressionTree =
    type Operator =
        | Sum
        | Subtraction
        | Division
        | Multiplication

    type TreeContent =
        | TreeOperator of Operator
        | TreeOperand of float
        | Nothing

    type Tree =
        | Node of Operator * Tree * Tree
        | Operand of float
        | Empty

    type ContinuationStep =
        | Finished
        | Step of TreeContent * (unit -> ContinuationStep)

    let differencePrecision = 0.1

    let rec linearize exprTree cont =
        match exprTree with
        | Empty -> Step(Nothing, fun () -> Finished)
        | Operand x -> Step(TreeOperand(x), cont)
        | Node(x, l, r) -> Step(TreeOperator(x), (fun () -> linearize l (fun () -> linearize r cont)))

    let calc exprTree =
        let steps = linearize exprTree (fun () -> Finished)

        let rec stackCalc stack =
            match stack with
            | TreeOperand(x) :: TreeOperand(y) :: TreeOperator(operator) :: tail ->
                match operator with
                | Sum -> (TreeOperand(x + y) :: tail)
                | Multiplication -> stackCalc (TreeOperand(x * y) :: tail)
                | Subtraction -> stackCalc (TreeOperand(y - x) :: tail)
                | Division ->
                    if (Math.Abs(x) > differencePrecision) then
                        stackCalc (TreeOperand(y / x) :: tail)
                    else
                        failwith "Division by zero"
            | _ -> stack

        let incorrectExprTreeThrow () =
            failwith "Incorrect expression in expression tree"

        let rec calcSteps step stack =
            match step with
            | Finished ->
                try
                    match stackCalc stack with
                    | [] -> 0.0
                    | result :: [] ->
                        match result with
                        | TreeOperand(x) -> x
                        | _ -> incorrectExprTreeThrow ()
                    | _ -> incorrectExprTreeThrow ()
                with Failure(msg) ->
                    failwith msg
            | Step(x, getNext) ->
                if (x = Nothing) then
                    calcSteps (getNext ()) stack
                else
                    try
                        calcSteps (getNext ()) (x :: stackCalc stack)
                    with Failure(msg) ->
                        failwith msg

        calcSteps steps []
