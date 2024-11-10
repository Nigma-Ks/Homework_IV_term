module PointFree.Tests

open NUnit.Framework
open FsCheck
open PointFree


[<Test>]
let ``Functions are equal`` () =
    let functionResAreEq (x: int) (l: int list) = (func'1 x l = func'2 x l) &&
                                                    (func'2 x l = func'3 x l) && (func'3 x l = func'4 x l) &&
                                                      (func'4 x l = func'5 x l)
    
    Check.Quick functionResAreEq