namespace TreeMap

module TreeMapFunc =
    type BinaryTree<'a> =
        | Node of 'a * BinaryTree<'a> * BinaryTree<'a>
        | Empty

    let map f binTree =
        let rec mapInternal f binTree =
            match binTree with
            | Node(x, l, r) -> Node(f x, mapInternal f l, mapInternal f r)
            | Empty -> Empty

        mapInternal f binTree
