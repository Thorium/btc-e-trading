module TestArbitrage

open NUnit.Framework

open BtceApiFramework.PrivateBtceApi
open BtceApiFramework.Currency
open BtceApiFramework

open Arbitrage
open MockDownloader

let getVertexForCurrency (vertices: Vertex list) (currency: Currency) : Vertex =
    List.find (fun vertex -> vertex.currency = currency) vertices

let edgesArePairs (vertex: Vertex) (pairs: Pair list) : bool =
    let matches = [ for pair in pairs do 
                        yield List.exists (fun edge -> edge.currencyPair = pair) vertex.edges ]
    not <| List.exists (fun m -> m = false) matches

[<TestFixture>]
type TestCurrency() = class

    [<Test>]
    member self.createArbitrageGraph() = 
        let getInfo = fun() -> PublicBtceApi.getInfoWithCustomDownloader mockDownloader

        let getPriceQuotes = PublicBtceApi.getPriceQuotesWithCustomDownloader mockDownloader

        let graph = createGraph getInfo getPriceQuotes

        let vertices = graph.vertices

        let btc = getVertexForCurrency vertices Currency.BTC
        Assert.AreEqual(3, btc.edges.Length)
        Assert.True(edgesArePairs btc [(Currency.BTC, Currency.USD);(Currency.BTC, Currency.EUR);(Currency.LTC, Currency.BTC)])

        let usd = getVertexForCurrency vertices Currency.USD
        Assert.AreEqual(3, usd.edges.Length)
        Assert.True(edgesArePairs usd [(Currency.BTC, Currency.USD);(Currency.LTC, Currency.USD);(Currency.EUR, Currency.USD)])

        let eur = getVertexForCurrency vertices Currency.EUR
        Assert.AreEqual(2, eur.edges.Length)
        Assert.True(edgesArePairs eur [(Currency.EUR, Currency.USD);(Currency.BTC, Currency.EUR)])

        let ltc = getVertexForCurrency vertices Currency.LTC
        Assert.AreEqual(2, ltc.edges.Length)
        Assert.True(edgesArePairs ltc [(Currency.LTC, Currency.USD);(Currency.LTC, Currency.BTC)])

    [<Test>]
    member self.getShortestPath() = 
        Assert.Fail()
end