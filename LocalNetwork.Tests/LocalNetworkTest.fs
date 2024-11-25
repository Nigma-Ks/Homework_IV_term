module LocalNetwork.Tests

open NUnit.Framework
open FsUnit
open Moq
open Network.LocalNetwork

let adjacencyMatrix =
    array2D
        [| [| false; true; false; true; false; false |] // Компьютер 1 соединен с 2 и 4
           [| true; false; true; false; false; false |] // Компьютер 2 соединен с 1 и 3
           [| false; true; false; true; false; false |] // Компьютер 3 соединен с 2 и 4
           [| true; false; true; false; true; false |] // Компьютер 4 соединен с 1, 3 и 5
           [| false; false; false; true; false; true |] // Компьютер 5 соединен с 4 и 6
           [| false; false; false; false; true; false |] |] // Компьютер 6 соединен с 5

let smallAdjacencyMatrix =
    array2D
        [| [| false; true; true |] // Компьютер 1 соединен с 2 и 3
           [| true; false; false |] // Компьютер 2 соединен с 1
           [| false; false; false |] |] // Компьютер 3 соединен с 1


[<Test>]
let ``Probability is zero test`` () =
    let windowsProb = Probability(0.0)
    let linuxProb = Probability(0.0)
    let macosProb = Probability(0.0)

    let computer1 =
        { OS = Windows(windowsProb)
          IsInfected = false }
        :> IComputer

    let computer2 =
        { OS = Linux(linuxProb)
          IsInfected = false }
        :> IComputer

    let computer3 =
        { OS = MacOS(macosProb)
          IsInfected = true }
        :> IComputer

    let computer4 =
        { OS = Windows(windowsProb)
          IsInfected = false }
        :> IComputer

    let computer5 =
        { OS = Linux(linuxProb)
          IsInfected = false }
        :> IComputer

    let computer6 =
        { OS = MacOS(macosProb)
          IsInfected = false }
        :> IComputer

    let computersArray =
        [| computer1; computer2; computer3; computer4; computer5; computer6 |]

    let network =
        { AdjacencyMatrix = adjacencyMatrix
          Computers = computersArray
          IsInfectedNetwork = false }

    network.InfectionSpreading.GetInfectedComputers.Length |> should equal 1

[<Test>]
let ``Probability is one test`` () =
    let windowsProb = Probability(1.0)
    let linuxProb = Probability(1.0)
    let macosProb = Probability(1.0)

    let computer1 =
        { OS = Windows(windowsProb)
          IsInfected = false }
        :> IComputer

    let computer2 =
        { OS = Linux(linuxProb)
          IsInfected = false }
        :> IComputer

    let computer3 =
        { OS = MacOS(macosProb)
          IsInfected = false }
        :> IComputer

    let computer4 =
        { OS = Windows(windowsProb)
          IsInfected = true }
        :> IComputer

    let computer5 =
        { OS = Linux(linuxProb)
          IsInfected = false }
        :> IComputer

    let computer6 =
        { OS = MacOS(macosProb)
          IsInfected = false }
        :> IComputer

    let computersArray =
        [| computer1; computer2; computer3; computer4; computer5; computer6 |]

    let network =
        { AdjacencyMatrix = adjacencyMatrix
          Computers = computersArray
          IsInfectedNetwork = false }

    let fstInfectionStepNetwork = modelInfectionSpreadStep network

    fstInfectionStepNetwork.GetInfectedComputers
    |> should
        equal
        [| computer1.CloneWithInfection
           computer3.CloneWithInfection
           computer4.CloneWithInfection
           computer5.CloneWithInfection |]

    let sndInfectionStepNetwork = modelInfectionSpreadStep fstInfectionStepNetwork

    sndInfectionStepNetwork.GetInfectedComputers.Length |> should equal 6

[<Test>]
let ``Infecting depends on willBeInfected result`` () =
    let mockComputer1 = new Mock<IComputer>()
    let mockComputer2 = new Mock<IComputer>()
    let mockComputer2Infected = new Mock<IComputer>()
    let mockComputer3Infected = new Mock<IComputer>()
    let mockComputer3 = new Mock<IComputer>()

    mockComputer2
        .Setup(fun x -> x.CloneWithInfection)
        .Returns(
            mockComputer2Infected.Object
        )
    |> ignore

    mockComputer2.Setup(fun x -> x.WillBeInfected).Returns(false) |> ignore
    mockComputer2.Setup(fun x -> x.IsInfected).Returns(false) |> ignore

    mockComputer3
        .Setup(fun x -> x.CloneWithInfection)
        .Returns(mockComputer3Infected.Object)
    |> ignore

    mockComputer3.Setup(fun x -> x.WillBeInfected).Returns(true) |> ignore
    mockComputer3.Setup(fun x -> x.IsInfected).Returns(false) |> ignore

    mockComputer1.Setup(fun x -> x.IsInfected).Returns(true) |> ignore

    let network =
        { AdjacencyMatrix = smallAdjacencyMatrix
          Computers = [| mockComputer1.Object; mockComputer2.Object; mockComputer3.Object |]
          IsInfectedNetwork = false }

    let fstInfectionStepNetwork = modelInfectionSpreadStep network

    fstInfectionStepNetwork.Computers
    |> should equal [| mockComputer1.Object; mockComputer2.Object; mockComputer3Infected.Object |]
