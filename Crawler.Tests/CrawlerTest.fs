module Crawler.Tests

open NUnit.Framework
open FsUnit
open Moq
open Сrawler.Сrawler
open System.Net.Http

let informationInTestUrl =
    "<div class=\"><a href=\"http://example1\">\\\<a href=\"http://example2\">\""

let fstLink = "<a href=\"http://example1\">"
let sndLink = "<a href=\"http://example2\">"

[<Test>]
let ``Incorrect url test`` () =
    let mockClient = new Mock<IHttpClient>()

    mockClient
        .Setup(fun c -> c.GetStringAsync(It.IsAny<string>()))
        .Throws<HttpRequestException>()
    |> ignore

    let result =
        downloadPagesFrom (mockClient.Object, "test_url") |> Async.RunSynchronously

    result.IsNone |> should be True

[<Test>]
let ``Correct input — correct result`` () =
    let mockClient = new Mock<IHttpClient>()

    mockClient
        .Setup(fun c -> c.GetStringAsync("test_url"))
        .ReturnsAsync(informationInTestUrl)
    |> ignore

    mockClient.Setup(fun c -> c.GetStringAsync(fstLink)).ReturnsAsync("text link 1")
    |> ignore

    mockClient.Setup(fun c -> c.GetStringAsync(sndLink)).ReturnsAsync("text link 2")
    |> ignore

    let result =
        downloadPagesFrom (mockClient.Object, "test_url") |> Async.RunSynchronously

    result.IsNone |> should be False
    let resultValue = result.Value

    let expected =
        seq {
            (fstLink, 11)
            (sndLink, 11)
        }

    resultValue |> should equal expected

[<Test>]
let ``Returns downloaded without errors links information test`` () =
    let mockClient = new Mock<IHttpClient>()

    mockClient
        .Setup(fun c -> c.GetStringAsync("test_url"))
        .ReturnsAsync(informationInTestUrl)
    |> ignore

    mockClient.Setup(fun c -> c.GetStringAsync(fstLink)).ReturnsAsync("text link 1")
    |> ignore

    mockClient
        .Setup(fun c -> c.GetStringAsync(sndLink))
        .Throws<HttpRequestException>()
    |> ignore

    let result =
        downloadPagesFrom (mockClient.Object, "test_url") |> Async.RunSynchronously

    result.IsNone |> should be False
    let resultValue = result.Value

    let expected = seq { (fstLink, 11) }

    resultValue |> should equal expected
