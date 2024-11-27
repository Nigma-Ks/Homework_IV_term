namespace Lazy

open System.Threading

module Lazy =
    /// Interface of Lazy.
    type ILazy<'a> =
        /// Returns new calculation of supplier value on first call.
        /// <returns>Result of supplier.</returns>
        abstract member Get: unit -> 'a

    /// Represents Lazy working with one thread.
    type СonsistentLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None

        interface ILazy<'a> with
            /// <inheritdoc/>
            member this.Get() =
                match value with
                | None ->
                    value <- Some(supplier ())
                    value.Value
                | Some x -> x

    /// Represents Lazy working with multithreads using lock.
    type LockLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None
        let lockObj = obj ()

        interface ILazy<'a> with
            /// <inheritdoc/>
            member this.Get() =
                lock (lockObj) (fun () ->
                    match value with
                    | None ->
                        value <- Some(supplier ())
                        value.Value
                    | Some x -> x)

    /// Represents Lazy working with multithreads using lock free.
    type LockFreeLazy<'a>(supplier: unit -> 'a) =
        let mutable value: Option<'a> = None

        interface ILazy<'a> with
            /// <inheritdoc/>
            member this.Get() =
                match Interlocked.CompareExchange(&value, Some(supplier ()), None) with
                | Some x -> x
                | None -> value.Value
