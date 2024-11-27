namespace Network

open System

module LocalNetwork =
    /// Random for module.
    let random = Random()

    /// Represents probability with value restrictions.
    type Probability(probability: float) =
        do
            if probability < 0.0 || probability > 1.0 then
                failwith "Probability must be between 0 and 1."

        /// Get probability.
        /// <returns>Probability.</returns>
        member this.Probability = probability

    /// Represents OS with probability of infection.
    type OperatingSystem =
        | Windows of Probability
        | Linux of Probability
        | MacOS of Probability

        /// Get probability.
        /// <returns>Probability of infection for OS.</returns>
        member x.GetProbability =
            match x with
            | Windows(probability) -> probability.Probability
            | Linux(probability) -> probability.Probability
            | MacOS(probability) -> probability.Probability

    /// Computer interface.
    type IComputer =
        /// Get computer's OS.
        /// <returns>OS.</returns>
        abstract member OS: OperatingSystem
        /// Is computer infected or not.
        /// <returns>True if infected false otherwise.</returns>
        abstract member IsInfected: bool
        /// Get if computer will be infected or not.
        /// <returnsReturns true with probability of infection for computer's OS.</returns>
        abstract member WillBeInfected: bool
        /// Get new infected IComputer.
        /// <returns>IComputer.</returns>
        abstract member CloneWithInfection: IComputer

    /// Represents computer.
    type Computer =
        { OS: OperatingSystem
          IsInfected: bool }

        interface IComputer with
            /// <inheritdoc/>
            member this.OS = this.OS
            /// <inheritdoc/>
            member this.IsInfected = this.IsInfected

            /// <inheritdoc/>
            member x.WillBeInfected =
                let randValue = random.NextDouble()
                randValue < x.OS.GetProbability

            /// <inheritdoc/>
            member x.CloneWithInfection = { OS = x.OS; IsInfected = true } :> IComputer

    /// Represents network.
    type Network =
        { AdjacencyMatrix: bool[,]
          Computers: IComputer array
          IsInfectedNetwork: bool }

    let difference = 0.001

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
        network.Computers |> Array.filter (fun x -> x.IsInfected)

    let getImmuneHealthyComputers (network: Network) =
        network.Computers
        |> Array.filter (fun (x: IComputer) -> x.OS.GetProbability < difference && not (x.IsInfected))

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
        /// Spreads infection throught computers in network while it possible.
        /// <returns>New infectioned network.</returns>
        member x.InfectionSpreading() = infectionSpreading x
        /// Returns infected computers of network.
        /// <returns>Infected computers.</returns>
        member x.InfectedComputers = getInfectedComputers x
        /// Returns computers with immunity (probability of infection is zero) of network.
        /// <returns>Computers with immunity.</returns>
        member x.ImmuneHealthyComputers = getImmuneHealthyComputers x
