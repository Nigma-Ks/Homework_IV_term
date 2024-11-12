namespace Network

open System

module LocalNetwork =
    let difference = 0.001

    type Probability(probability: float) =
        do if probability < 0.0 || probability > 1.0 then
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

    type Computer = {
        OS: OperatingSystem;
        IsInfected: bool
    }

    type Network = {
        AdjacencyMatrix: bool[,];
        Computers: Computer array
    }

    let cloneWithInfection (computer: Computer) = {computer with IsInfected = true}

    let willBeInfected (computer: Computer) =
        let random = Random()
        let randValue = random.NextDouble()
        randValue < computer.OS.GetProbability

    type Computer with
        member x.WillBeInfected = willBeInfected x
        member x.CloneWithInfection = cloneWithInfection x

    let rec infectConnectedWithN (i: int) (computerNumber: int) (adjacencyMatrix: bool[,]) (computers: Computer array) =
        if i < computers.Length then
            if (adjacencyMatrix.[computerNumber, i])
                then
                    if computers.[i].WillBeInfected then
                        Array.set computers i computers.[i].CloneWithInfection
                        infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
                    else infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
            else infectConnectedWithN (i + 1) computerNumber adjacencyMatrix computers
        else computers

    let getInfectedComputers (network: Network) =
        Array.filter(fun x -> x.IsInfected) <| network.Computers

    let getImmuneComputers (network: Network) =
        Array.filter(fun x -> x.OS.GetProbability < difference) <| network.Computers

    type Network with
        member x.getInfectedComputers = getInfectedComputers x
        member x.getImmuneComputers = getImmuneComputers x

    let modelInfectionSpreadStep (network: Network) =
        if (network.getInfectedComputers.Length + network.getImmuneComputers.Length = network.Computers.Length) then network
        else
            let computers = Array.copy (network.Computers)
            let rec infectComputers (i: int) (computers: Computer array) (newComputersArray: Computer array)  =
                if i < (computers.Length) then
                    if (computers.[i]).IsInfected then
                        infectComputers (i + 1) computers (infectConnectedWithN 0 i network.AdjacencyMatrix newComputersArray)
                    else infectComputers (i + 1) computers newComputersArray
                else newComputersArray
            {network with Computers = infectComputers 0 (network.Computers) computers}

    type Network with
        member x.ModelInfectionStep = modelInfectionSpreadStep x



