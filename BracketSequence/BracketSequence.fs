namespace BracketSequence

module BracketSequenceChecker =
    let bracketList = [ ('(', ')'); ('[', ']'); ('{', '}') ]

    let isOpen (bracket: char) =
        bracket = '(' || bracket = '{' || bracket = '['

    let isClose (bracket: char) =
        bracket = ']' || bracket = ')' || bracket = '}'

    let getRelativeOpen (bracket: char) =
        List.find (fun x -> snd x = bracket) bracketList |> fst

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
            | [] -> if bracketsStack = [] then true else false

        internalBracketSequenceChecker (bracketString |> Seq.toList) []
