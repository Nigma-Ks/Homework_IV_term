module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter


[<Test>]
let ``Terms without redux stay same`` () =
      let term = Application (Variable ("y"), LambdaAbstraction ("x", Variable ("x")))

      betaReduction term |> should equal <| term

[<Test>]
let ``Correct substitutions`` () =
     let term = Application
                    (Application
                        (LambdaAbstraction ("x",
                            LambdaAbstraction ("z", Variable ("z"))),
                                Variable ("y")),
                                    Variable ("y"))

     let res = betaReduction term

     res |> should equal <| Variable ("y")

[<Test>]
let ``Correct alpha conversion`` () =
    let term = Application
                    (LambdaAbstraction
                        ("x", LambdaAbstraction
                                ("y", Application
                                    (Variable ("y"), Variable ("x")))), Variable ("y"))

    let res = betaReduction term

    res |> should equal <| LambdaAbstraction ("y'", Application (Variable ("y'"), Variable ("y")))

