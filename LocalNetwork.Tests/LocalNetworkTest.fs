module LocalNetwork.Tests

open NUnit.Framework
open FsUnit
open LocalNetwork

let adjacencyMatrix =
    array2D [|
        [| false; false;  false; false; false;  false |]
        [| false;  false; false;  false;  false; false |]
        [| false; false; false;  false; false;  false  |]
        [| false;  false;  false; false; false; false  |]
        [| false; false;  false;  false; false; false |]
        [| false;  false; false; false;  false;  false |]
    |]

//[<Test>]
//let ``Probability is zero test`` =
//    let windows = Windows(0.0)
//    let linux = Linux(0.0)
//    let macOS = MacOS(0.0)
//    let computer1 = {OperatingSystem = windows; }
//    let computers = array[{}]


    

