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
    member self.getPaths() = 
        let getInfo = fun() -> PublicBtceApi.getInfoWithCustomDownloader mockDownloader

        let getPriceQuotes = PublicBtceApi.getPriceQuotesWithCustomDownloader mockDownloader

        let graph = createGraph getInfo getPriceQuotes

        let paths = paths (adjacencyListForCurrency Currency.BTC graph) graph

        // Debug
        for path in paths do
            Console.WriteLine("")
            for edge in path do
                Console.Write(currencyPairToString(edge.currencyPair) + ", ")
            Console.Write((pathProfit path).ToString())

    [<Test>]
    member self.showLiveData() = 
        let graph = createGraph PublicBtceApi.getInfo PublicBtceApi.getPriceQuotes

        let currencies: Currency array = Enum.GetValues(typedefof<Currency>) :?> Currency array

        let currencies =
            Async.Parallel [ for currency in currencies -> 
                                async { return paths (adjacencyListForCurrency currency graph) graph } ]
            |> Async.RunSynchronously

        // Debug
        for paths in currencies do
            Console.WriteLine("\n")
            for path in paths do
                Console.WriteLine("")
                for edge in path do
                    Console.Write(currencyPairToString(edge.currencyPair) + ", ")
                Console.Write((pathProfit path).ToString())
end