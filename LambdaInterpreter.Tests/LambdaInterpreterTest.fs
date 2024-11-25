module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter


[<Test>]
let ``Terms without redux stay same test`` () =
    let term = Application(Variable("y"), LambdaAbstraction("x", Variable("x"))) //y lx.x

    betaReduction term |> should equal <| term

[<Test>]
let ``Correct substitutions test`` () =
    let term =
        Application(
            Application(LambdaAbstraction("x", LambdaAbstraction("z", Variable("z"))), Variable("y")),
            Variable("y")
        ) // (lx.lz.z) y y

    let res = betaReduction term

    res |> should equal <| Variable("y")

[<Test>]
let ``Correct alpha conversion test`` () =
    let term =
        Application(
            LambdaAbstraction("x", LambdaAbstraction("y", Application(Variable("y"), Variable("x")))),
            Variable("y")
        ) // (lx.ly. y x) y

    let res = betaReduction term

    res |> should equal
    <| LambdaAbstraction("y'", Application(Variable("y'"), Variable("y")))

[<Test>]
let ``Check S K K = I test`` () =
    let sInternal =
        Application(Application(Variable("x"), Variable("z")), Application(Variable("y"), Variable("z")))

    let s =
        LambdaAbstraction("x", LambdaAbstraction("y", LambdaAbstraction("z", sInternal)))

    let k = LambdaAbstraction("x", LambdaAbstraction("y", Variable("x")))

    let term = Application(Application(s, k), k)
    let i = LambdaAbstraction("z", Variable("z"))

    let res = betaReduction term

    res |> should equal <| i

[<Test>]
let ``Check term reduction on fst hw expression test`` () =
    let term =
        Application(
            Application(
                LambdaAbstraction(
                    "a",
                    Application(
                        LambdaAbstraction("b", Application(Variable("b"), Variable("b"))),
                        LambdaAbstraction("b", Application(Variable("b"), Variable("b")))
                    )
                ),
                Variable("b")
            ),
            Application(
                LambdaAbstraction("c", Application(Variable("c"), Variable("b"))),
                LambdaAbstraction("a", Variable("a"))
            )
        )

    let res = betaReduction term

    let rightResult =
        Application(
            Application(
                LambdaAbstraction("b", Application(Variable("b"), Variable("b"))),
                LambdaAbstraction("b", Application(Variable("b"), Variable("b")))
            ),
            Variable("b")
        )

    res |> should equal <| rightResult
