module EvenNumbersCounter.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open EvenNumbersCounters

[<Test>]
let ``MapCounter function counts right`` () =
      mapCounter [1; 2; 3; 4] |> should equal 2

[<Test>]
let ``FoldCounter function counts right`` () =
      foldCounter [1; 2; 3; 4] |> should equal 2

[<Test>]
let ``FilterCounter function counts right`` () =
      filterCounter [1; 2; 3; 4] |> should equal 2

[<Test>]
let ``FilterCounter, mapCounter and foldCounter are equivalent`` () =
    let countersResultsEq (ls: list<int>) = (mapCounter ls = foldCounter ls) && (mapCounter ls = filterCounter ls)

    Check.Quick countersResultsEq