(*
    Copyright (C) 2013  Matthew Mcveigh

    This file is part of F# Unaffiliated BTC-E Trading Framework.

    F# Unaffiliated BTC-E Trading Framework is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    F# Unaffiliated BTC-E Trading Framework is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with F# Unaffiliated BTC-E Trading Framework. If not, see <http://www.gnu.org/licenses/>.
*)

module TestArbitrage

open NUnit.Framework

open BtceApiFramework.PrivateBtceApi
open BtceApiFramework.Currency
open BtceApiFramework

open TradingFramework.Arbitrage
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
    0m

[<TestFixture>]
type TestCurrency() = class
    (*
        Exchange rate calculations for mock data:

        btc->usd=95.351 - (95.351 * (0.2 / 100))            usd->btc=(1.0 / 95.7) - ((1.0 / 95.7) * (0.2 / 100))
        btc->eur=75.30001 - (75.30001 * (0.2 / 100))        eur->btc=(1.0 / 75.71001) - ((1.0 / 75.71001) * (0.2 / 100))
        ltc->btc=0.02732 - (0.02732 * (0.2 / 100))          btc->ltc=(1.0 / 0.02734) - ((1.0 / 0.02734) * (0.2 / 100))
        ltc->usd=2.610012 - (2.610012 * (0.2 / 100))        usd->ltc=(1.0 / 2.62608) - ((1.0 / 2.62608) * (0.2 / 100))
        eur->usd=1.25713 - (1.25713 * (0.2 / 100))          usd->eur=(1.0 / 1.2677) - ((1.0 / 1.2677) * (0.2 / 100))
    *)
    [<Test>]
    member self.createArbitrageGraph() = 
        let getInfo = fun() -> PublicBtceApi.getInfoWithCustomDownloader mockDownloader

        let getPriceQuotes = PublicBtceApi.getPriceQuotesWithCustomDownloader mockDownloader

        let graph = createGraph getInfo getPriceQuotes

        let vertices = graph.adjacencyLists

        let btc = getAdjacencyListForCurrency vertices Currency.BTC
        Assert.AreEqual(3, btc.edges.Length)
        Assert.True(edgesArePairs btc [(Currency.BTC, Currency.USD);(Currency.BTC, Currency.EUR);(Currency.LTC, Currency.BTC)])

        let getEdgeForPair (edges: Edge list) (pair: Pair) : Edge =
            List.find (fun x -> x.currencyPair = pair) edges

        // btc->usd
        Assert.AreEqual(95.351m - (95.351m * (0.2m / 100m)), (getEdgeForPair btc.edges (Currency.BTC, Currency.USD)).exchangeRate)
        // btc->eur
        Assert.AreEqual(75.30001m - (75.30001m * (0.2m / 100m)), (getEdgeForPair btc.edges (Currency.BTC, Currency.EUR)).exchangeRate)
        // btc->ltc
        Assert.AreEqual((1m / 0.02734m) - ((1m / 0.02734m) * (0.2m / 100m)), (getEdgeForPair btc.edges (Currency.LTC, Currency.BTC)).exchangeRate)

        let usd = getAdjacencyListForCurrency vertices Currency.USD
        Assert.AreEqual(3, usd.edges.Length)
        Assert.True(edgesArePairs usd [(Currency.BTC, Currency.USD);(Currency.LTC, Currency.USD);(Currency.EUR, Currency.USD)])

        // usd->btc
        Assert.AreEqual((1m / 95.7m) - ((1m / 95.7m) * (0.2m / 100m)), (getEdgeForPair usd.edges (Currency.BTC, Currency.USD)).exchangeRate)
        // usd->ltc
        Assert.AreEqual((1m / 2.62608m) - ((1m / 2.62608m) * (0.2m / 100m)), (getEdgeForPair usd.edges (Currency.LTC, Currency.USD)).exchangeRate)
        // usd->eur
        Assert.AreEqual((1m / 1.2677m) - ((1m / 1.2677m) * (0.2m / 100m)), (getEdgeForPair usd.edges (Currency.EUR, Currency.USD)).exchangeRate)

        let eur = getAdjacencyListForCurrency vertices Currency.EUR
        Assert.AreEqual(2, eur.edges.Length)
        Assert.True(edgesArePairs eur [(Currency.EUR, Currency.USD);(Currency.BTC, Currency.EUR)])

        // eur->usd
        Assert.AreEqual(1.25713m - (1.25713m * (0.2m / 100m)), (getEdgeForPair eur.edges (Currency.EUR, Currency.USD)).exchangeRate)
        // eur->btc
        Assert.AreEqual((1m / 75.71001m) - ((1m / 75.71001m) * (0.2m / 100m)), (getEdgeForPair eur.edges (Currency.BTC, Currency.EUR)).exchangeRate)

        let ltc = getAdjacencyListForCurrency vertices Currency.LTC
        Assert.AreEqual(2, ltc.edges.Length)
        Assert.True(edgesArePairs ltc [(Currency.LTC, Currency.USD);(Currency.LTC, Currency.BTC)])

        // ltc->usd
        Assert.AreEqual(2.610012m - (2.610012m * (0.2m / 100m)), (getEdgeForPair ltc.edges (Currency.LTC, Currency.USD)).exchangeRate)
        // ltc->btc
        Assert.AreEqual(0.02732m - (0.02732m * (0.2m / 100m)), (getEdgeForPair ltc.edges (Currency.LTC, Currency.BTC)).exchangeRate)

    [<Test>]
    member self.getPaths() = 
        let getInfo = fun() -> PublicBtceApi.getInfoWithCustomDownloader mockDownloader

        let getPriceQuotes = PublicBtceApi.getPriceQuotesWithCustomDownloader mockDownloader

        let graph = createGraph getInfo getPriceQuotes

        let paths = paths (adjacencyListForCurrency Currency.BTC graph) graph 3

        // Debug
        for path in paths do
            Console.WriteLine("")
            let mutable i = 1m
            for edge in path do
                Console.Write(currencyPairToString(edge.currencyPair) + " " + edge.exchangeRate.ToString() + ", ")
            Console.Write((pathProfit path).ToString())

    [<Test>]
    member self.showLiveData() = 
        let graph = createGraph PublicBtceApi.getInfo PublicBtceApi.getPriceQuotes

        let currencies: Currency array = Enum.GetValues(typedefof<Currency>) :?> Currency array

        let currencies =
            Async.Parallel [ for currency in currencies -> 
                                async { return paths (adjacencyListForCurrency currency graph) graph 4 } ]
            |> Async.RunSynchronously

        // Debug
        for paths in currencies do
            Console.WriteLine("\n")
            for path in List.sortBy (fun x -> pathProfit x) paths do
                Console.WriteLine("")
                for edge in path do
                    Console.Write(currencyPairToString(edge.currencyPair) + ", ")
                Console.Write((pathProfit path).ToString())
end