module PhoneBook.Tests

open NUnit.Framework
open FsUnit
open System.IO
open PhoneBook

let testFilePath = "testFileBase.txt"

[<Test>]
let ``Write and read test`` () =
    File.WriteAllText(testFilePath,"")
    let data = [({Name = "Max"; Phone = "123"}); ({Name = "Max"; Phone = "123"})]
    writeContactsToFile data testFilePath
    readContactsFromFile testFilePath |> should equal data

[<Test>]
let ``Add new record test`` () =
    let data = [({Name = "Max"; Phone = "123"})]
    let expected = ([({Name = "Vasya"; Phone = "124"}); ({Name = "Max"; Phone = "123"})], true)
    addContactToBase "Vasya" "124" data |> should equal expected

[<Test>]
let ``Can not add record with existed in base phone or name`` () =
    let data = [({Name = "Max"; Phone = "123"})]
    let (newData, _) = addContactToBase "Vasya" "123" data
    let (newNewData, _) = addContactToBase "Max" "345" newData
    (newNewData, newData) |> should equal (data, data)

[<Test>]
let ``Finding ny name and phone test`` () =
    let name = "Max"
    let phone = "123"
    let contact = ({Name = name; Phone = phone})
    let data = [contact]
    let res1 = findContactByNameInBase name data
    let res2 = findContactByPhoneInBase phone data

    (res1, res2) |> should equal (Some(contact), Some(contact))

[<Test>]
let ``isContactInFile test`` () =
    let name = "Max"
    let phone = "123"
    File.WriteAllText(testFilePath,"")
    let data = [({Name = name; Phone = phone})]
    writeContactsToFile data testFilePath
    isContactInFile name phone testFilePath |> should be True

[<Test>]
let ``isContactInBase test`` () =
    let data = [({Name = "Max"; Phone = "123"})]

    isContactInBase "Max" "123" data |> should be True
    
[<Test>]
let ``Write contacts from base to file test`` () =
    File.WriteAllText(testFilePath, "")
    let data = [({Name = "Max"; Phone = "123"})]
    writeContactsToFile data testFilePath
    let contacts = [({Name = "Vasya"; Phone = "124"}); ({Name = "Max"; Phone = "123"})]
    writePhoneBaseToFile contacts testFilePath
    readContactsFromFile testFilePath |> should equal contacts

[<Test>]
let ``Write contacts from file to base`` () =
    File.WriteAllText(testFilePath,"")
    let data = [({Name = "Max"; Phone = "123"}); ({Name = "Vasya"; Phone = "124"})]
    let contacts = [({Name = "Max"; Phone = "123"})]
    writeContactsToFile data testFilePath

    writePhoneBaseFromFile contacts testFilePath |> should equal data

[<Test>]
let ``Correct phone test`` () =
    (isCorrectPhone "88005555", isCorrectPhone "880055d5")|> should equal (true, false)