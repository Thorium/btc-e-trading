module TestArbitrage

open NUnit.Framework

open BtceApiFramework.PrivateBtceApi
open BtceApiFramework.Currency
open BtceApiFramework

open Arbitrage
open MockDownloader

open System

let getAdjacencyListForCurrency (adjacencyLists: AdjacencyList list) (currency: Currency) : AdjacencyList =
    List.find (fun adjacencyList -> adjacencyList.vertex.currency = currency) adjacencyLists

let edgesArePairs (adjacencyList: AdjacencyList) (pairs: Pair list) : bool =
    let matches = [ for pair in pairs do 
                        yield List.exists (fun edge -> edge.currencyPair = pair) adjacencyList.edges ]
    not <| List.exists (fun m -> m = false) matches

let getAllPaths (vertex: Vertex) : (Edge list) list =
    []

let getValueFromPath (path: Edge list) : Decimal =
    new Decimal(0)

[<TestFixture>]
type TestCurrency() = class

    [<Test>]
    member self.createArbitrageGraph() = 
        let getInfo = fun() -> PublicBtceApi.getInfoWithCustomDownloader mockDownloader

        let getPriceQuotes = PublicBtceApi.getPriceQuotesWithCustomDownloader mockDownloader

        let graph = createGraph getInfo getPriceQuotes

        let vertices = graph.adjacencyLists

        let btc = getAdjacencyListForCurrency vertices Currency.BTC
        Assert.AreEqual(3, btc.edges.Length)
        Assert.True(edgesArePairs btc [(Currency.BTC, Currency.USD);(Currency.BTC, Currency.EUR);(Currency.LTC, Currency.BTC)])

        let usd = getAdjacencyListForCurrency vertices Currency.USD
        Assert.AreEqual(3, usd.edges.Length)
        Assert.True(edgesArePairs usd [(Currency.BTC, Currency.USD);(Currency.LTC, Currency.USD);(Currency.EUR, Currency.USD)])

        let eur = getAdjacencyListForCurrency vertices Currency.EUR
        Assert.AreEqual(2, eur.edges.Length)
        Assert.True(edgesArePairs eur [(Currency.EUR, Currency.USD);(Currency.BTC, Currency.EUR)])

        let ltc = getAdjacencyListForCurrency vertices Currency.LTC
        Assert.AreEqual(2, ltc.edges.Length)
        Assert.True(edgesArePairs ltc [(Currency.LTC, Currency.USD);(Currency.LTC, Currency.BTC)])

    [<Test>]
    member self.getShortestPath() = 
        Assert.Fail()
end