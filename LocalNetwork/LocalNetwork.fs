namespace Network

open System

module LocalNetwork =
    let difference = 0.001

    type Probability(probability: float) =
        do
            if probability < 0.0 || probability > 1.0 then
                failwith "Probability must be between 0 and 1."

        member this.Probability = probability

    type OperatingSystem =
        | Windows of Probability
        | Linux of Probability
        | MacOS of Probability

        member x.GetProbability =
            match x with
            | Windows(probability) -> probability.Probability
            | Linux(probability) -> probability.Probability
            | MacOS(probability) -> probability.Probability

    type IComputer =
        abstract member OS: OperatingSystem
        abstract member IsInfected: bool
        abstract member WillBeInfected: bool
        abstract member CloneWithInfection: IComputer

    type Computer =
        { OS: OperatingSystem
          IsInfected: bool }

        interface IComputer with
            member this.OS = this.OS
            member this.IsInfected = this.IsInfected

            member x.WillBeInfected =
                let random = Random()
                let randValue = random.NextDouble()
                randValue < x.OS.GetProbability

            member x.CloneWithInfection = { OS = x.OS; IsInfected = true } :> IComputer

    type Network =
        { AdjacencyMatrix: bool[,]
          Computers: IComputer array
          IsInfectedNetwork: bool }

    let rec infectConnectedWithN
        (i: int)
        (computerNumber: int)
        (adjacencyMatrix: bool[,])
        (computers: IComputer array)
        =
        if i < computers.Length then
            if adjacencyMatrix.[computerNumber, i] then
                if not (computers.[i].IsInfected) && computers.[i].WillBeInfected then
                    Array.set computers i (computers.[i].CloneWithInfection)
                    infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
                else
                    infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
            else
                infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
        else
            computers

    let getInfectedComputers (network: Network) =
        Array.filter (fun (x: IComputer) -> x.IsInfected) <| network.Computers

    let getImmuneHealthyComputers (network: Network) =
        Array.filter (fun (x: IComputer) -> x.OS.GetProbability < difference && not (x.IsInfected))
        <| network.Computers

    let modelInfectionSpreadStep (network: Network) =
        let computers = Array.copy (network.Computers)

        let rec infectComputers (i: int) (computers: IComputer array) (newComputersArray: IComputer array) =
            if i < (computers.Length) then
                if (computers.[i]).IsInfected then
                    infectComputers
                        (i + 1)
                        computers
                        (infectConnectedWithN 0 i network.AdjacencyMatrix newComputersArray)
                else
                    infectComputers (i + 1) computers newComputersArray
            else
                newComputersArray

        let newComputers = infectComputers 0 (network.Computers) computers

        { network with
            Computers = newComputers }

    let rec infectionSpreading (network: Network) =
        if
            getInfectedComputers(network).Length + getImmuneHealthyComputers(network).Length = network.Computers.Length
        then
            { network with
                IsInfectedNetwork = true }
        else
            infectionSpreading <| modelInfectionSpreadStep network

    type Network with
        member x.InfectionSpreading = infectionSpreading x
        member x.GetInfectedComputers = getInfectedComputers x
        member x.GetImmuneHealthyComputers = getImmuneHealthyComputers x
