namespace BracketSequence

module BracketSequenceChecker =
    let bracketList = Map [(')', '('); ('}', '{'); (']', '[')]

    let isOpen (bracket: char) =
        bracket = '(' || bracket = '{' || bracket = '['

    let isClose (bracket: char) =
        bracket = ']' || bracket = ')' || bracket = '}'

    let getRelativeOpen (bracket: char) = bracketList.[bracket]

    let isCorrectBracketSequence (bracketString: string) =
        let rec internalBracketSequenceChecker charList bracketsStack =
            match charList with
            | fstChar :: tail ->
                if isOpen fstChar then
                    internalBracketSequenceChecker tail (fstChar :: bracketsStack)
                elif isClose fstChar then
                    match bracketsStack with
                    | lastBracket :: brackets ->
                        if lastBracket = getRelativeOpen (fstChar) then
                            internalBracketSequenceChecker tail brackets
                        else
                            false
                    | _ -> false
                else
                    internalBracketSequenceChecker tail bracketsStack
            | [] -> bracketsStack = []

        internalBracketSequenceChecker (bracketString |> Seq.toList) []
