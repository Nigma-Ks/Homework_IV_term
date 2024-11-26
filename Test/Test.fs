namespace Test

module Test =
    /// finds min element in list if possible
    /// <param name="ls">list in which we trying to find min element</param>
    /// <returns>min element</returns>
    let findMinElement ls =
        match ls with
        | h :: tail -> Some <| List.fold (fun acc x -> if x < acc then x else acc) h tail
        | _ -> None

    /// represents BinaryTree
    type BinaryTree<'a> =
        | Node of 'a * BinaryTree<'a> * BinaryTree<'a>
        | Empty

    /// represents Continuation steps with tree on currentStep and intermediate int calculation
    type ContinuationStep<'a> =
        | Finished
        | Step of BinaryTree<'a> * int * (unit -> ContinuationStep<'a>)

    /// linearizes binary tree traversal using steps of ContinuationStep
    /// <param name="binTree">tree to find traversal</param>
    /// <param name="currDistance">distance from root to current Node to store in step</param>
    /// <param name="currDistance">continuation function</param>
    /// <returns>step</returns>
    let rec linearize binTree currDistance cont =
        match binTree with
        | Empty -> cont ()
        | Node(_, l, r) ->
            Step(
                binTree,
                currDistance,
                (fun () -> linearize l (currDistance + 1) (fun () -> linearize r (currDistance + 1) cont))
            )

    /// finds if possible min distance from node to root in tree
    /// <param name="binTree">tree where we want to find min distance</param>
    /// <returns>min distance</returns>
    let findMinDistance binTree =
        if binTree = Empty then
            None
        else
            let steps = linearize binTree 0 (fun () -> Finished)

            let rec processSteps step distanceList =
                match step with
                | Finished -> distanceList
                | Step(x, currDistance, getNext) ->
                    match x with
                    | Node(_, Empty, Empty) -> processSteps (getNext ()) (currDistance :: distanceList)
                    | _ -> processSteps (getNext ()) distanceList

            findMinElement (processSteps steps [])

    /// represents hash-table
    type HashTable<'a when 'a: equality>(size: int, hashFunction: 'a -> int) =
        let mutable table = Array.init size (fun _ -> [])

        let rec removeElement element list =
            match list with
            | [] -> []
            | head :: tail when head = element -> tail
            | head :: tail -> head :: (removeElement element tail)

        let getHashIndex element =
            let hash = hashFunction element
            hash % size

        /// adds element to hash-table by hash index, erases
        /// <param name="element">element</param>
        member this.Add(element) =
            let index = getHashIndex element
            table.[index] <- element :: table.[index]

        /// checks if element is in hash-table
        /// <param name="element">element</param>
        /// <returns>true if element in hash-table false otherwise</returns>
        member this.Contains(element) =
            let index = getHashIndex element
            List.contains element table.[index]

        /// removes element from hash-table
        /// <param name="element">element</param>
        /// <returns>true if element was in hash-table (was removed) false otherwise</returns>
        member this.Remove(element) =
            let index = getHashIndex element

            match this.Contains element with
            | false -> false
            | _ ->
                table.[index] <- removeElement element table.[index]
                true
