module PrimeNumberSeq.Tests

open NUnit.Framework
open FsUnit
open PrimeNumberSeqGenerator

[<Test>]
let ``IsPrime returns correct values test`` () =
    [ 1; 2; 3; 4; 5 ]
    |> List.map (fun x -> isPrime x)
    |> should equal [ false; true; true; false; true ]

[<Test>]
let ``Generator returns correct numbers, test first 6 test`` () =
    createPrimeNumbersInfSeq ()
    |> Seq.take 6
    |> Seq.toList
    |> should equal [ 2; 3; 5; 7; 11; 13 ]
