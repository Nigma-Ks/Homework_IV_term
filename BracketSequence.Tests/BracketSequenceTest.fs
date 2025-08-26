module BracketSequence.Tests

open NUnit.Framework
open FsUnit
open BracketSequenceChecker

[<Test>]
let ``Function checks correctly sequence of brackets`` () =
    let brSeqCorr = "(({})[])"
    let brSeqIncorr = "((({})[])"

    (isCorrectBracketSequence brSeqCorr, isCorrectBracketSequence brSeqIncorr)
    |> should equal (true, false)

[<Test>]
let ``Function checks correctly sequence of brackets with other symbols`` () =
    let brSeq = "(t({}d)[]g)"

    isCorrectBracketSequence brSeq |> should be True

[<Test>]
let ``Function checks correctly empty string`` () =
    isCorrectBracketSequence "" |> should be True

[<Test>]
let ``Function checks correctly string with no braskets`` () =
    let brSeq = "abc"

    isCorrectBracketSequence brSeq |> should be True
