module Network.Tests

open NUnit.Framework
open FsUnit
open LocalNetwork

let adjacencyMatrix = 
    array2D [|
        [| false; true; false; true; false; false |]  // Компьютер 1 соединен с 2 и 4
        [| true; false; true; false; false; false |]  // Компьютер 2 соединен с 1 и 3
        [| false; true; false; true; false; false |]  // Компьютер 3 соединен с 2 и 4
        [| true; false; true; false; true; false |]   // Компьютер 4 соединен с 1, 3 и 5
        [| false; false; false; true; false; true |]  // Компьютер 5 соединен с 4 и 6
        [| false; false; false; false; true; false |]  // Компьютер 6 соединен с 5
    |]

[<Test>]
let ``Probability is zero test``() =
    let windowsProb = Probability(0.0) 
    let linuxProb = Probability(0.0)
    let macosProb = Probability(0.0)
    let computer1 = { OS = Windows(windowsProb); IsInfected = false }
    let computer2 = { OS = Linux(linuxProb); IsInfected = true }  
    let computer3 = { OS = MacOS(macosProb); IsInfected = false }  
    let computer4 = { OS = Windows(windowsProb); IsInfected = false } 
    let computer5 = { OS = Linux(linuxProb); IsInfected = true } 
    let computer6 = { OS = MacOS(macosProb); IsInfected = false } 
    let computersArray = [| computer1; computer2; computer3; computer4; computer5; computer6 |]

    let network = { AdjacencyMatrix = adjacencyMatrix; Computers = computersArray }

    network.ModelInfectionStep |> should equal network

[<Test>]
let ``Max probability behavior corresponds to breadth first traversal test``() =
    let windowsProb = Probability(1.0) 
    let linuxProb = Probability(1.0)
    let macosProb = Probability(1.0)
    let computer1 = { OS = Windows(windowsProb); IsInfected = false } 
    let computer2 = { OS = Linux(linuxProb); IsInfected = false}
    let computer3 = { OS = MacOS(macosProb); IsInfected = false } 
    let computer4 = { OS = Windows(windowsProb); IsInfected = true } 
    let computer5 = { OS = Linux(linuxProb); IsInfected = false }
    let computer6 = { OS = MacOS(macosProb); IsInfected = false }
    let computersArray = [| computer1; computer2; computer3; computer4; computer5; computer6 |]

    let infectedComputer1 = computer1.CloneWithInfection
    let infectedComputer3 = computer3.CloneWithInfection
    let infectedComputer5 = computer5.CloneWithInfection

    let nextComputersArray = [| infectedComputer1; computer2; infectedComputer3; computer4; infectedComputer5; computer6 |]

    let network = { AdjacencyMatrix = adjacencyMatrix; Computers = computersArray}

    network.ModelInfectionStep |> should equal {network with Computers = nextComputersArray}