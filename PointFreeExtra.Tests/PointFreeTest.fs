module PointFreeExtra.Tests

open NUnit.Framework
open FsCheck
open PointFree

[<Test>]
let ``Functions are  equal test`` () =
    let equalityChecker (g: ('a -> 'b)) (l: 'a list) =
        if (l.IsEmpty) then
            true
        else
            f'1 g l = f'2 g l
            && f'3 g l = f'4 g l
            && f'5 g l = (f'6<'a, 'b> g l)
            && f'1 g l = f'4 g l
            && f'1 g l = f'5 g l

    Check.QuickThrowOnFailure equalityChecker
