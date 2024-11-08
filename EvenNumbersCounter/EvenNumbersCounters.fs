namespace EvenNumbersCounter

module EvenNumbersCounters =
    let mapCounter list = List.sum <|
                            List.map (fun x -> if x % 2 = 0 then 1
                                                   else 0) list

    let foldCounter list = List.fold (fun acc elem -> acc + if elem % 2 = 0 then 1
                                                                else 0) 0 list

    let filterCounter list = List.length <| List.filter (fun x -> x % 2 = 0) list