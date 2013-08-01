module TestArbitrage

open NUnit.Framework

open BtceApiFramework.PrivateBtceApi
open BtceApiFramework

open Arbitrage

[<TestFixture>]
type TestCurrency() = class

    [<Test>]
    member self.createArbitrageGraph() = 
        let graph = createGraph PublicBtceApi.getInfo PublicBtceApi.getPriceQuotes 

        let m = graph.vertices.Head.edges.Head

        System.Console.WriteLine(graph.vertices.Head.currency.ToString())

        System.Console.WriteLine(m.ask.ToString())
        System.Console.WriteLine(m.sell.ToString())
        System.Console.WriteLine(m.transactionFee.ToString())
        System.Console.WriteLine(Currency.currencyPairToString(m.currencyPair))

        let m = graph.vertices.Tail.Head.edges.Head

        System.Console.WriteLine(graph.vertices.Tail.Head.currency.ToString())

        System.Console.WriteLine(m.ask.ToString())
        System.Console.WriteLine(m.sell.ToString())
        System.Console.WriteLine(m.transactionFee.ToString())
        System.Console.WriteLine(Currency.currencyPairToString(m.currencyPair))

        let m = graph.vertices.Tail.Head.edges.Tail.Head

        System.Console.WriteLine(m.ask.ToString())
        System.Console.WriteLine(m.sell.ToString())
        System.Console.WriteLine(m.transactionFee.ToString())
        System.Console.WriteLine(Currency.currencyPairToString(m.currencyPair))
end