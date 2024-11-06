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
        | Operand of float

    type Tree =
        | Node of TreeContent * Tree * Tree
        | Empty

    type ContinuationStep =
        | Finished
        | Step of TreeContent * (unit -> ContinuationStep)

    let differencePrecision = 0.1

    let rec linearize exprTree cont =
        match exprTree with
            | Empty -> cont()
            | Node (x, l, r) ->
                Step (x, (fun () ->
                    linearize l (fun () ->
                        linearize r cont)))

    let calc exprTree =
        let steps = linearize exprTree (fun () -> Finished)

        let rec stackCalc stack =
            match stack with
                | Operand(x) :: Operand(y) :: TreeOperator(operator) :: tail ->
                    match operator with
                        | Sum -> (Operand(x + y) :: tail)
                        | Multiplication -> stackCalc (Operand(x * y) :: tail)
                        | Subtraction -> stackCalc (Operand(y - x) :: tail)
                        | Division ->
                            if (Math.Abs(x) > differencePrecision) then stackCalc (Operand(y / x) :: tail)
                            else failwith "Division by zero"
                | _ -> stack

        let incorrectExprTreeThrow () = failwith "Incorrect expression in expression tree"

        let rec calcSteps step stack =
            match step with
                | Finished ->
                    try
                        match stackCalc stack with
                            | [] -> 0.0
                            | result :: [] ->
                                match result with
                                    | Operand(x) -> x
                                    | _ -> incorrectExprTreeThrow()
                            | _ -> incorrectExprTreeThrow()
                    with
                        | Failure(msg) -> failwith msg
                | Step (x, getNext) ->
                    try
                        calcSteps (getNext()) (x :: (stackCalc stack))
                    with
                        | Failure(msg) -> failwith msg
        calcSteps steps []