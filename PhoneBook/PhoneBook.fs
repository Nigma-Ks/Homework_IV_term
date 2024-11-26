namespace PhoneBook

open System.IO
open System.Text.RegularExpressions
open Newtonsoft.Json

module PhoneBook =
    type Contact = { Name: string; Phone: string }

    let readContactsFromFile phoneBaseFile =
        if File.Exists(phoneBaseFile) then
            let json = File.ReadAllText(phoneBaseFile)

            if json = "" then
                []
            else
                JsonConvert.DeserializeObject<Contact list>(json)
        else
            []

    let writeContactsToFile (contacts: Contact list) phoneBaseFile =
        if contacts = [] then
            ()
        else
            let json =
                JsonConvert.SerializeObject((contacts @ (readContactsFromFile phoneBaseFile)), Formatting.Indented)

            File.WriteAllText(phoneBaseFile, json)

    let isContactInFile nameForFind phoneToFind phoneBaseFile =
        List.exists (fun contact -> contact.Name = nameForFind || contact.Phone = phoneToFind)
        <| readContactsFromFile phoneBaseFile

    let isContactInBase nameForFind phoneToFind phoneBase =
        List.exists (fun contact -> contact.Name = nameForFind || contact.Phone = phoneToFind) phoneBase

    let writePhoneBaseFromFile phoneBaseFile = readContactsFromFile phoneBaseFile

    let isCorrectPhone (phone: string) =
        let pattern = @"^[0-9]+$"
        let regex = new Regex(pattern)
        regex.IsMatch(phone)

    let addContactToBase name phone (phoneBase: Contact List) =
        if (not (isContactInBase name phone phoneBase)) && isCorrectPhone (phone) then
            (({ Name = name; Phone = phone } :: phoneBase), true)
        else
            (phoneBase, false)

    let findContactByPhoneInBase phone (phoneBase: Contact List) =
        List.tryFind (fun contact -> contact.Phone = phone) phoneBase

    let findContactByNameInBase name (phoneBase: Contact List) =
        List.tryFind (fun contact -> contact.Name = name) phoneBase
