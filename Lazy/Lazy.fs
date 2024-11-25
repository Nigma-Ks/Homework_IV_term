namespace Lazy

open System.Threading

module Lazy =
    type ILazy<'a> =
        abstract member Get: unit -> 'a

    type СonsistentLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None

        interface ILazy<'a> with
            member this.Get() =
                match value with
                | None ->
                    value <- Some(supplier ())
                    value.Value
                | Some x -> x

    type LockLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None
        let lockObj = obj ()

        interface ILazy<'a> with
            member this.Get() =
                Monitor.Enter(lockObj)

                try
                    match value with
                    | None ->
                        value <- Some(supplier ())
                        value.Value
                    | Some x -> x
                finally
                    Monitor.Exit(lockObj)

    type FreeLockLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None

        interface ILazy<'a> with
            member this.Get() =
                match Interlocked.CompareExchange(&value, Some(supplier ()), None) with
                | Some x -> x
                | None -> value.Value
