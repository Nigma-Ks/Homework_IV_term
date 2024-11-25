namespace LambdaInterpreter

module LambdaInterpreter =

    type LambdaTerm =
        | Variable of string
        | Application of LambdaTerm * LambdaTerm
        | LambdaAbstraction of string * LambdaTerm

    let rec IsFV term var =
        match term with
        | Variable x -> x = var
        | Application(fstTerm, sndTerm) -> (IsFV fstTerm var) || (IsFV sndTerm var)
        | LambdaAbstraction(x, internalTerm) -> if x = var then false else IsFV internalTerm var

    let rec alphaConversion term oldVar newVar =
        match term with
        | Variable x -> if x = oldVar then Variable newVar else term
        | Application(fstTerm, sndTerm) ->
            Application(alphaConversion fstTerm oldVar newVar, alphaConversion sndTerm oldVar newVar)
        | LambdaAbstraction(x, internalTerm) ->
            if x = oldVar then
                if IsFV internalTerm newVar then
                    term
                else
                    LambdaAbstraction(newVar, alphaConversion internalTerm oldVar newVar)
            else
                LambdaAbstraction(x, alphaConversion internalTerm oldVar newVar)

    let rec substitution termForChange substitutionVar substitutionTerm =
        match termForChange with
        | Variable y ->
            if substitutionVar = y then
                substitutionTerm
            else
                termForChange
        | Application(fstTerm, sndTerm) ->
            Application(
                substitution fstTerm substitutionVar substitutionTerm,
                substitution sndTerm substitutionVar substitutionTerm
            )
        | LambdaAbstraction(y, internalTerm) ->
            if substitutionVar = y || not (IsFV internalTerm substitutionVar) then
                termForChange
            else if IsFV substitutionTerm y then
                let newY = y + "'"
                let newInternalTerm = alphaConversion internalTerm y newY
                LambdaAbstraction(newY, substitution newInternalTerm substitutionVar substitutionTerm)
            else
                LambdaAbstraction(y, substitution internalTerm substitutionVar substitutionTerm)

    let rec internalBetaReduction term =
        match term with
        | Variable _ -> term
        | LambdaAbstraction(var, internalTerm) -> LambdaAbstraction(var, internalBetaReduction internalTerm)
        | Application(fstTerm, sndTerm) ->
            match fstTerm with
            | LambdaAbstraction(var, internalTerm) -> substitution internalTerm var sndTerm
            | Variable _ -> Application(fstTerm, internalBetaReduction sndTerm)
            | Application _ ->
                let reducedFstTerm = internalBetaReduction fstTerm

                if reducedFstTerm = fstTerm then
                    Application(fstTerm, internalBetaReduction sndTerm)
                else
                    Application(reducedFstTerm, sndTerm)

    let rec betaReduction term =
        let reducedTerm = internalBetaReduction term

        if reducedTerm = term then
            term
        else
            betaReduction reducedTerm
