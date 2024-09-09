module PrimeNumberSeq.Tests

open NUnit.Framework
open FsUnit
open PrimeNumberSeqGenerator

[<Test>]
let ``First 6 generated prime numbers should be correct`` () =
     createPrimeNumbersInfSeq () |> Seq.take 6 |> Seq.toList |> should equal [2; 3; 5; 7; 11; 13]