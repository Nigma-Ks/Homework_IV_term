module Workflow.Tests

open NUnit.Framework
open FsUnit
open Workflow

[<Test>]
let ``With numbers calculating test`` () =
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    result |> should equal <| Some(3)

[<Test>]
let ``With incorrect numbers calculating test`` () =
    let result = calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }
    result |> should equal <| None

[<Test>]
let ``Rounding test`` () =
    let result = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    result |> should equal <| 0.048


