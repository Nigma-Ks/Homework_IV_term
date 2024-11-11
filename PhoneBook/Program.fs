open System
open PhoneBook.PhoneBook

let printInstructions() =
    printfn "It's a phone book!"
    printfn "You can enter command number to:"
    printfn "1. Add new record"
    printfn "2. Find contact by phone"
    printfn "3. Find contact by name"
    printfn "4. Print current phone book"
    printfn "5. Print contacts from file"
    printfn "6. Add contacts from file to current phone book"
    printfn "7. Add contacts from current phone book to file"
    printfn "8. Exit"

let phoneBaseFile = "PhoneBase.txt"

let stringContact contact = sprintf "%s - %s" contact.Name contact.Phone

let printContactList (contactList: Contact list) =
    if List.isEmpty contactList then 
        printfn "There are no contacts\n"
    else
        printfn "n"
        for contact in contactList do
            printfn "%s" (stringContact contact)

let rec enterCorrectPhone() =
    printfn "Enter phone: "
    let phone = Console.ReadLine()
    if isCorrectPhone(phone) then phone
    else
        printfn "Phone was incorrect\n"
        enterCorrectPhone()

let addNewRecord (phoneBase: Contact list) =
    printfn "To add new record enter name: "
    let name = Console.ReadLine()
    let phone = enterCorrectPhone()
    match addContactToBase name phone phoneBase with
    | (newPhoneBase, true) ->
        printfn "Contact added successfully!\n"
        newPhoneBase
    | _ ->
        printfn "Contact wasn't added: phone or name are already in phone book or\n"
        phoneBase

let printFindingRes (result: Contact option) =
    match result with
    | Some contact ->
        printfn "Contact was found: %s" (stringContact contact)
    | None ->
        printfn "Contact wasn't found"

let findPhoneByName (phoneBase: Contact list) =
    let phone = enterCorrectPhone()
    printFindingRes (findContactByPhoneInBase phone phoneBase)

let findNameByPhone (phoneBase: Contact list) =
    printfn "Enter name: "
    let name = Console.ReadLine()
    printFindingRes (findContactByNameInBase name phoneBase)

let printCurrentPhoneBook (phoneBase: Contact list) =
    printfn "Current phone book:\n"
    printContactList phoneBase

let printContactsFromFile () =
    printfn "Contacts in file:"
    printContactList (readContactsFromFile phoneBaseFile)

let addContactsFromFile (phoneBase: Contact list) = 
    writePhoneBaseFromFile phoneBase phoneBaseFile

let addContactsToFile (phoneBase: Contact list) = 
    writePhoneBaseToFile phoneBase phoneBaseFile

let Run =
    let rec internalRun (phoneBase: Contact list) =
        printInstructions()
        printfn "Enter command: "
        let command = Console.ReadLine()
        match command with
        | "1" ->
            let newPhoneBase = addNewRecord phoneBase
            internalRun newPhoneBase
        | "2" ->
            findPhoneByName phoneBase
            internalRun phoneBase
        | "3" ->
            findNameByPhone phoneBase
            internalRun phoneBase
        | "4" ->
            printCurrentPhoneBook phoneBase
            internalRun phoneBase
        | "5" ->
            printContactsFromFile()
            internalRun phoneBase
        | "6" ->
            let newPhoneBase = addContactsFromFile phoneBase
            internalRun newPhoneBase
        | "7" ->
            addContactsToFile phoneBase
            internalRun phoneBase
        | "8" ->
            printfn "Exit"
        | _ ->
            printfn "Incorrect command!\n"
            internalRun phoneBase
    internalRun []