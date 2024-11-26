module PhoneBook.Tests

open NUnit.Framework
open FsUnit
open System.IO
open PhoneBook

let testFilePath = "testFileBase.txt"

let contactMax = ({ Name = "Max"; Phone = "123" })
let contactVasya = ({ Name = "Vasya"; Phone = "124" })

[<Test>]
let ``Write and read test`` () =
    File.WriteAllText(testFilePath, "")
    let data = [ contactMax; contactMax ]
    writeContactsToFile data testFilePath
    readContactsFromFile testFilePath |> should equal data

[<Test>]
let ``Add new record test`` () =
    let data = [ contactMax ]

    let expected = ([ contactVasya; contactMax ], true)

    addContactToBase "Vasya" "124" data |> should equal expected

[<Test>]
let ``Can not add record with existed in base phone or name`` () =
    let data = [ contactMax ]
    let (newData, _) = addContactToBase "Vasya" "123" data
    let (newNewData, _) = addContactToBase "Max" "345" newData
    (newNewData, newData) |> should equal (data, data)

[<Test>]
let ``Finding ny name and phone test`` () =
    let name = "Max"
    let phone = "123"
    let data = [ contactMax ]
    let res1 = findContactByNameInBase name data
    let res2 = findContactByPhoneInBase phone data

    (res1, res2) |> should equal (Some(contactMax), Some(contactMax))

[<Test>]
let ``isContactInFile test`` () =
    let name = "Max"
    let phone = "123"
    File.WriteAllText(testFilePath, "")
    let data = [ contactMax ]
    writeContactsToFile data testFilePath
    isContactInFile name phone testFilePath |> should be True

[<Test>]
let ``isContactInBase test`` () =
    let data = [ contactMax ]

    isContactInBase "Max" "123" data |> should be True

[<Test>]
let ``Write contacts from base to file test`` () =
    File.WriteAllText(testFilePath, "")

    let phoneBase = [ contactVasya; contactMax ]

    writeContactsToFile phoneBase testFilePath
    readContactsFromFile testFilePath |> should equal phoneBase

[<Test>]
let ``Write contactMaxs from file to base`` () =
    File.WriteAllText(testFilePath, "")

    let data = [ contactMax; contactVasya ]

    writeContactsToFile data testFilePath

    writePhoneBaseFromFile testFilePath |> should equal data

[<Test>]
let ``Correct phone test`` () =
    (isCorrectPhone "88005555", isCorrectPhone "880055d5")
    |> should equal (true, false)
