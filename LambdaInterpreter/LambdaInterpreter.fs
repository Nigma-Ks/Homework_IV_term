namespace LambdaInterpreter

module LambdaInterpreter =

    type LambdaTerm =
        | Variable of string
        | Application of LambdaTerm * LambdaTerm
        | LambdaAbstraction of string * LambdaTerm

    let rec isVarInTerm term var =
        match term with
        | Variable x -> if x = var then true else false
        | Application(fstTerm, sndTerm) -> (isVarInTerm fstTerm var) || (isVarInTerm sndTerm var)
        | LambdaAbstraction(x, internalTerm) -> if x = var then true else isVarInTerm internalTerm var

    let rec isFV term var =
        match term with
        | Variable x -> x = var
        | Application(fstTerm, sndTerm) -> (isFV fstTerm var) || (isFV sndTerm var)
        | LambdaAbstraction(x, internalTerm) -> if x = var then false else isFV internalTerm var

    let rec alphaConversion term oldVar newVar =
        match term with
        | Variable x -> if x = oldVar then Variable newVar else term
        | Application(fstTerm, sndTerm) ->
            Application(alphaConversion fstTerm oldVar newVar, alphaConversion sndTerm oldVar newVar)
        | LambdaAbstraction(x, internalTerm) ->
            if x = oldVar then
                if isFV internalTerm newVar then
                    term
                else
                    LambdaAbstraction(newVar, alphaConversion internalTerm oldVar newVar)
            else
                LambdaAbstraction(x, alphaConversion internalTerm oldVar newVar)

    let rec getNewName oldVar termWhereItShoulBeFree =
        let newVar = oldVar + "'"
        if isFV termWhereItShoulBeFree newVar || not (isVarInTerm termWhereItShoulBeFree newVar) then newVar
        else getNewName newVar termWhereItShoulBeFree

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
            if substitutionVar = y || not (isFV internalTerm substitutionVar) then
                termForChange
            else if isFV substitutionTerm y then
                let newY = getNewName y substitutionTerm
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
                Application(internalBetaReduction fstTerm, internalBetaReduction sndTerm)

    let rec betaReduction term =
        let reducedTerm = internalBetaReduction term

        if reducedTerm = term then
            term
        else
            betaReduction reducedTerm
