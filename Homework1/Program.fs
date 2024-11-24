
let findFirstOccurance x ls = 
    let rec findFirstOccuranceInternal index x ls = 
        if ls = [] then -1
        elif ls.Head = x then index
        else findFirstOccuranceInternal (index + 1) x ls.Tail
    findFirstOccuranceInternal 0 x ls

let createListOfTwoPowers n m =
    let rec raiseToPower number power result =
        if (power = 0) then result
        elif (power % 2 <> 0) then raiseToPower number (power - 1) (result * number)
        else raiseToPower (number * number) (power / 2) result
    List.scan(fun acc _ -> acc * 2) (raiseToPower 2 n 1) [ for i in n..(m + n - 1) -> i ]

let invertList () = Seq.fold (fun acc x -> x :: acc) []

let getNFibonacciNumber n = 
    let rec nFibonacciNumberInternal prevPrev prev n = 
        if n = 0 then prevPrev
        else nFibonacciNumberInternal prev (prevPrev + prev) (n - 1)
    nFibonacciNumberInternal 0 1 n

let getNFactorial n =
    let rec nFactorialInternal acc n = 
        if n = 0 then acc
        else nFactorialInternal (acc * n) (n - 1)
    nFactorialInternal 1 n
