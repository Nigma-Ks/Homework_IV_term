namespace TreeMap

module TreeMapFunc =
    type Direction =
        | Left
        | Right

    type BinaryTree<'a> =
        | Node of 'a * BinaryTree<'a> * BinaryTree<'a> 
        | Empty

    type ContinuationStep<'a> =
        | Finished
        | Step of 'a * list<Direction> * (unit -> ContinuationStep<'a>)
            
    let rec insertInTree tree directions x =
        match tree with
            | Empty -> Node(x, Empty, Empty)
            | Node (y, l, r) ->
                match directions with
                    | h :: tail ->
                        match h with
                            | Left -> Node (y, insertInTree l tail x, r)
                            | Right -> Node (y, l, insertInTree r tail x)
                    | [] -> Node(x, Empty, Empty)

    let rec linearizeDir directions binTree cont =
         match binTree with
             | Empty -> cont()
             | Node (x, l, r) ->
                 Step (x, directions, (fun () ->
                     linearizeDir (Left :: directions) l (fun () ->
                         linearizeDir (Right :: directions) r cont)))
    
    let map f binTree =
        let steps =
                linearizeDir [] binTree (fun () -> Finished)

        let rec processSteps step currTree =
            match step with
                | Finished -> currTree
                | Step (x, directions, getNext) ->
                        processSteps <| getNext() <| insertInTree currTree directions (f x)
        processSteps steps Empty

    //equality

    type ContinuationStepForEquality<'a> =
        | Finished
        | Step of 'a * (unit -> ContinuationStepForEquality<'a>)

    let rec linearize binTree cont =
        match binTree with
            | Empty -> cont()
            | Node (x, l, r) ->
                Step (x, (fun () ->
                    linearize l (fun () ->
                        linearize r cont)))

    let equalityTreeChecker binTree1 binTree2 =
        let stepsForFst =
            linearize binTree1 (fun () -> Finished)

        let stepsForSnd =
            linearize binTree2 (fun () -> Finished)

        let rec stepsEqualityChecker stepsForFst stepsForSnd =
            match stepsForFst with
                | Finished ->
                    match stepsForSnd with
                    | Finished -> true
                    | _ -> false
                | Step (x, getNextFst) ->
                    match stepsForSnd with
                        | Finished -> false
                        | Step(y, getNextSnd) ->
                            if x = y then stepsEqualityChecker <| getNextFst() <| getNextSnd()
                            else false
        stepsEqualityChecker stepsForFst stepsForSnd