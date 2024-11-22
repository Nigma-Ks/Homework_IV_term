namespace Workflow

open System

module Workflow =
    let toNumber (str: string) =
            match System.Int32.TryParse str with
            | true, int -> Some int
            | _ -> None

    type CalculationBuilder() =
        member this.Bind(x:string, f) =
            match toNumber(x) with
                | None -> None
                | Some a -> f a
        member this.Return(x) =
            Some x
    let calculate = CalculationBuilder();

    type RoundingCalculationBuilder(precision: int) =
        member this.Bind(x: float, f) =
            f <| Math.Round(x, precision)
        member this.Return(x: float) =
            Math.Round(x, precision)

    let rounding = RoundingCalculationBuilder