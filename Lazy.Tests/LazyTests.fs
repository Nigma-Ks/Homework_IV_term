﻿module Lazy.Tests

open NUnit.Framework
open FsCheck
open FsUnit
open System.Threading
open Lazy

[<Test>]
let ``Consistent Lazy returns same calculation result`` () =
    let consistentLazyChecker (supplier: unit -> int) =
        let newLazy = СonsistentLazy<int>(supplier) :> ILazy<int>
        let fstResult = newLazy.Get()
        let sndResult = newLazy.Get()
        let thrdResult = newLazy.Get()
        fstResult = sndResult && sndResult = thrdResult

    Check.QuickThrowOnFailure consistentLazyChecker

let lazies =
    [ (fun supplier -> LockLazy(supplier) :> ILazy<int>)
      (fun supplier -> LockFreeLazy(supplier) :> ILazy<int>) ]
    |> List.map (fun x -> TestCaseData(x))

[<TestCaseSource("lazies")>]
let ``Lock and free lock Lazies execute supplier one time`` (newILazy: (unit -> int) -> ILazy<int>) =
    let event = new ManualResetEvent(false)
    let mutable count = ref 0

    let multiThreadLazy =
        newILazy (fun () ->
            Interlocked.Increment count)

    let task =
        async {
            event.WaitOne() |> ignore
            return multiThreadLazy.Get()
        }

    let taskSeq = Seq.init 10 (fun _ -> task) |> Async.Parallel
    Thread.Sleep(100)
    event.Set() |> ignore
    let results = taskSeq |> Async.RunSynchronously
    let mutable isEqual = true

    for result in results do
        isEqual <- (result = results.[0]) && isEqual

    isEqual |> should be True
