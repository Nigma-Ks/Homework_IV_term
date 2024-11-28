namespace TreeIterator

module TreeIterator =
    /// Represents AVL tree node.
    type AVLNode<'a>(value: 'a) =
        let mutable value = value
        let mutable left: AVLNode<'a> option = None
        let mutable right: AVLNode<'a> option = None
        let mutable height = 1

        /// Value of node.
        member this.Value
            with get () = value
            and set (newValue) = value <- newValue

        /// Node's left node.
        member this.Left
            with get () = left
            and set (newLeft) = left <- newLeft

        /// Node's right node.
        member this.Right
            with get () = right
            and set (newRight) = right <- newRight

        /// Node height.
        member this.Height
            with get () = height
            and set (newHeight) = height <- newHeight

    /// represents Continuation steps with node value on currentStep
    type ContinuationStep<'a> =
        | Finished
        | Step of 'a * (unit -> ContinuationStep<'a>)

    /// linearizes tree traversal using steps of ContinuationStep.
    /// <param name="AVLTree">Tree to find traversal.</param>
    /// <returns>Step.</returns>
    let rec linearize (avlTree: AVLNode<'a> option) cont =
        match avlTree with
        | None -> cont ()
        | Some node -> Step(node.Value, (fun () -> linearize node.Left (fun () -> linearize node.Right cont)))

    /// Represents enumerator.
    type AVLTreeEnumerator<'a>(tree: AVLNode<'a> option) =
        let mutable currentStep = linearize tree (fun () -> Finished)
        let mutable currentStepValue: 'a option = None

        /// Standart IEnumerator<'a> interface
        interface System.Collections.Generic.IEnumerator<'a> with
            member this.Current =
                match currentStepValue with
                | None -> failwith "Traversal was ended."
                | Some x -> x

            member this.Dispose() = ()

        /// Standart IEnumerator interface
        interface System.Collections.IEnumerator with
            member this.Current: obj =
                match currentStepValue with
                | None -> failwith "Traversal was ended."
                | Some x -> box x

            member this.MoveNext() : bool =
                match currentStep with
                | Finished -> false
                | Step(x, getNext) ->
                    currentStepValue <- Some x
                    let nextStep = getNext ()
                    currentStep <- nextStep
                    true

            member this.Reset() =
                currentStep <- linearize tree (fun () -> Finished)
                currentStepValue <- None


    /// Represents AVL tree.
    type AVLTree<'a when 'a: comparison>() =
        let mutable root: AVLNode<'a> option = None

        let height (node: AVLNode<'a> option) =
            match node with
            | Some n -> n.Height
            | None -> 0

        let NodeLRHeightDiff (node: AVLNode<'a> option) =
            match node with
            | Some n -> height n.Right - height n.Left
            | None -> 0

        let updateHeight (node: AVLNode<'a>) =
            node.Height <- 1 + max (height node.Left) (height node.Right)

        let rotateRight (y: AVLNode<'a>) =
            let x = y.Left.Value
            y.Left <- x.Right
            x.Right <- Some y
            updateHeight y
            updateHeight x
            Some x

        let rotateLeft (x: AVLNode<'a>) =
            let y = x.Right.Value
            x.Right <- y.Left
            y.Left <- Some x
            updateHeight x
            updateHeight y
            Some y

        let balance (node: AVLNode<'a>) =
            updateHeight node

            match NodeLRHeightDiff(Some node) with
            | diff when diff = -2 ->
                if NodeLRHeightDiff node.Left < 0 then
                    rotateRight node
                else
                    node.Left <- rotateLeft node.Left.Value
                    rotateRight node
            | diff when diff = 2 ->
                if NodeLRHeightDiff node.Right > 0 then
                    rotateLeft node
                else
                    node.Right <- rotateRight node.Right.Value
                    rotateLeft node
            | _ -> Some node

        let rec insert (node: AVLNode<'a> option) value =
            match node with
            | None -> Some(AVLNode(value))
            | Some node when value < node.Value ->
                node.Left <- insert node.Left value
                balance node
            | Some node when value > node.Value ->
                node.Right <- insert node.Right value
                balance node
            | _ -> node //this value is already in tree

        /// Inserts new value in tree.
        /// <param name="value">Value.</param>
        member this.Insert(value) = root <- insert root value

        /// Get tree root.
        /// <returns>Root.</returns>
        member this.Root = root

        /// Get enumerator.
        interface System.Collections.Generic.IEnumerable<'a> with
            member this.GetEnumerator() =
                new AVLTreeEnumerator<'a>(this.Root) :> System.Collections.Generic.IEnumerator<'a>

        interface System.Collections.IEnumerable with
            member this.GetEnumerator() =
                new AVLTreeEnumerator<'a>(this.Root) :> System.Collections.IEnumerator
