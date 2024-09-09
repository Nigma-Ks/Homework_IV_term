namespace PrimeNumberSeq

module PrimeNumberSeqGenerator =
    let createPrimeNumbersInfSeq () =
        let isPrime number =
                let sqrtNumber = int (sqrt(float number))
                let rec isPrimeInternal currDivider =
                    if currDivider > sqrtNumber then true
                    elif number % currDivider = 0 then false
                    else isPrimeInternal (currDivider + 1)
                isPrimeInternal 2
        let rec generatePrimes currentNumber =
            if isPrime currentNumber then
                seq {
                    yield currentNumber
                    yield! generatePrimes (currentNumber + 1)
                }
            else
                generatePrimes (currentNumber + 1)
        generatePrimes 2