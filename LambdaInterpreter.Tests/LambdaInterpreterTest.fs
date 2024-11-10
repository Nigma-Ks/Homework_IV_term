module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter


[<Test>]
let ``Terms without redux stay same`` () =
      let term = Application (Variable ("y"), LambdaAbstraction ("x", Variable ("x"))) //y lx.x

      betaReduction term |> should equal <| term

[<Test>]
let ``Correct substitutions`` () =
     let term = Application
                    (Application
                        (LambdaAbstraction ("x",
                            LambdaAbstraction ("z", Variable ("z"))),
                                Variable ("y")),
                                    Variable ("y")) // (lx.lz.z) y y

     let res = betaReduction term

     res |> should equal <| Variable ("y")

[<Test>]
let ``Correct alpha conversion`` () =
    let term = Application
                    (LambdaAbstraction
                        ("x", LambdaAbstraction
                                ("y", Application
                                    (Variable ("y"), Variable ("x")))), Variable ("y")) // (lx.ly. y x) y

    let res = betaReduction term

    res |> should equal <| LambdaAbstraction ("y'", Application (Variable ("y'"), Variable ("y")))

[<Test>]
let ``Check S K K = I`` () =
    let sInternal = Application (Application (Variable ("x"), Variable ("z")), Application (Variable ("y"), Variable ("z")))
    let s = LambdaAbstraction ("x", LambdaAbstraction ("y", LambdaAbstraction ("z", sInternal)))
    let k = LambdaAbstraction ("x", LambdaAbstraction ("y", Variable ("x")))

    let term = Application (Application (s, k), k)
    let i = LambdaAbstraction ("z", Variable ("z"))

    let res = betaReduction term

    res |> should equal <| i

[<Test>]
let ``Check term reduction`` () =
    let bbb = LambdaAbstraction ("b", Application( Variable ("b"), Variable ("b")))
    let bbb2 = Application (bbb, bbb)
    let ab = LambdaAbstraction("a", bbb2)
    let sndTerm = Application (LambdaAbstraction ("c", Application ( Variable ("c"), Variable ("b"))), LambdaAbstraction ("a", Variable ("a")))
    let term = Application (Application (ab, Variable ("b")), sndTerm)

    let res = betaReduction term

    res |> should equal <| Application (bbb2, Variable ("b"))