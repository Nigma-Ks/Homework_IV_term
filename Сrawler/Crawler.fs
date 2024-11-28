namespace Сrawler

open System.Text.RegularExpressions
open System.Net.Http
open System.Threading.Tasks

module Сrawler =
    type IHttpClient =
        abstract member GetStringAsync: string -> Task<string>

    let getHtmlLinks (urlContent: string) =
        let regex = new Regex(@"<a href=""http://[^""]*"">", RegexOptions.IgnoreCase)
        let matches = regex.Matches(urlContent) |> Seq.map (fun x -> x.Value)
        matches

    let downloadWebPageContent (client: IHttpClient, url: string) : Async<Option<string>> =
        async {
            try
                let! response = client.GetStringAsync(url) |> Async.AwaitTask
                return Some response
            with :? HttpRequestException ->
                printfn "Error downloading URL: %s" url
                return None
        }

    let printDownloadingResult (results: seq<string * int>) =
        for result in results do
            printfn "%s %d" <| "url: " + fst result + "\nAmount of symbols:" <| snd result

    let downloadPagesFrom (client: IHttpClient, url: string) =
        async {
            let! urlContent = downloadWebPageContent (client, url)

            match urlContent with
            | None -> return None
            | Some content ->
                let downloadingTasks =
                    getHtmlLinks content
                    |> Seq.map (fun link ->
                        async {
                            let! pageContent = downloadWebPageContent (client, link)
                            return link, pageContent
                        })
                    |> Async.Parallel

                let! results = downloadingTasks

                let unwrappedResults =
                    results
                    |> Seq.filter (fun (x: string * Option<string>) -> (snd x).IsSome)
                    |> Seq.map (fun (x: string * Option<string>) -> fst x, (snd x).Value.Length)

                printDownloadingResult unwrappedResults
                return Some unwrappedResults
        }
