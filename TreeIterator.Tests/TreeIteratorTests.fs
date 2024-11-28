﻿module TreeIterator.Tests

open NUnit.Framework
open FsUnit
open TreeIterator


let avlTree = AVLTree<int>()
let values = [ 10; 20; 30; 40; 50; 25 ]

for value in values do
    avlTree.Insert(value)

[<Test>]
let ``AVL tree inserts elements right`` () =

    avlTree.Root.Value.Value |> should equal 30

[<Test>]
let ``Iterator test`` () =
    let mutable valuesList = []

    for value in avlTree do
        valuesList <- value :: valuesList

    valuesList |> should equal [ 50; 40; 25; 10; 20; 30 ]