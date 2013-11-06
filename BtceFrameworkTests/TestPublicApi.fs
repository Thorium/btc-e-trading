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

module TestPublicApi

open NUnit.Framework

open System

open BtceApiFramework.PublicBtceApi
open BtceApiFramework.Currency

open MockDownloader

[<TestFixture>]
type TestPublicApi() = class

    [<Test>]
    member self.getTickerInfoOnPairs() = 
        let quotes = getPriceQuotesWithCustomDownloader mockDownloader [(Currency.BTC, Currency.USD); (Currency.LTC, Currency.USD)]
        let (pair, quote) = quotes.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.USD))
        Assert.AreEqual(quote.high, new Decimal(88.9))
        Assert.AreEqual(quote.low, new Decimal(86.82))
        Assert.AreEqual(quote.average, new Decimal(87.86))
        Assert.AreEqual(quote.tradingVolume, new Decimal(134881.26056))
        Assert.AreEqual(quote.tradingVolumeInCurrency, new Decimal(1536.15523))
        Assert.AreEqual(quote.lastTransactionPrice, new Decimal(88))
        Assert.AreEqual(quote.buy, new Decimal(88))
        Assert.AreEqual(quote.sell, new Decimal(87.7))
        Assert.AreEqual(quote.updated, (int64)1375005886)

        let (pair, quote) = quotes.Tail.Head
        Assert.AreEqual(pair, (Currency.LTC, Currency.USD))
        Assert.AreEqual(quote.high, new Decimal(2.692))
        Assert.AreEqual(quote.low, new Decimal(2.6502))
        Assert.AreEqual(quote.average, new Decimal(2.6711))
        Assert.AreEqual(quote.tradingVolume, new Decimal(54088.32538))
        Assert.AreEqual(quote.tradingVolumeInCurrency, new Decimal(20235.81144))
        Assert.AreEqual(quote.lastTransactionPrice, new Decimal(2.67191))
        Assert.AreEqual(quote.buy, new Decimal(2.671912))
        Assert.AreEqual(quote.sell, new Decimal(2.67191))
        Assert.AreEqual(quote.updated, (int64)1375005886)

    [<Test>]
    member self.getDepth() = 
        let depths = getDepthWithCustomDownloader mockDownloader [(Currency.BTC, Currency.USD); (Currency.LTC, Currency.USD)]
        let (pair, depth) = depths.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.USD))
        let asks = depth.asks
        let ask = asks.Head
        Assert.AreEqual(ask.amount, new Decimal(52.05465611))
        Assert.AreEqual(ask.price, new Decimal(88))
        let asks = asks.Tail
        let ask = asks.Head
        Assert.AreEqual(ask.amount, new Decimal(0.4))
        Assert.AreEqual(ask.price, new Decimal(88.14))
        let asks = asks.Tail
        let ask = asks.Head
        Assert.AreEqual(ask.amount, new Decimal(0.02))
        Assert.AreEqual(ask.price, new Decimal(88.147))

        let bid = depth.bids.Head
        Assert.AreEqual(bid.amount, new Decimal(8.28483479))
        Assert.AreEqual(bid.price, new Decimal(87.7))
        let bid = depth.bids.Tail.Head
        Assert.AreEqual(bid.amount, new Decimal(2.67559182))
        Assert.AreEqual(bid.price, new Decimal(87.693))

        let (pair, depth) = depths.Tail.Head
        Assert.AreEqual(pair, (Currency.LTC, Currency.USD))

    [<Test>]
    member self.getInfoOnPairs() = 
        let info = getInfoWithCustomDownloader mockDownloader
        let pairs = info.pairs

        Assert.AreEqual(info.serverTime, (int64)1374928864)

        let (pair, pairInfo) = pairs.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.USD))
        Assert.AreEqual(pairInfo.decimalPlaces, new Decimal(3))
        Assert.AreEqual(pairInfo.minPrice, new Decimal(0.1))
        Assert.AreEqual(pairInfo.maxPrice, new Decimal(400))
        Assert.AreEqual(pairInfo.minAmount, new Decimal(0.01))
        Assert.AreEqual(pairInfo.hidden, 0)
        Assert.AreEqual(pairInfo.fee, new Decimal(0.2))

        let pairs = pairs.Tail
        let (pair, pairInfo) = pairs.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.RUR))
        Assert.AreEqual(pairInfo.decimalPlaces, new Decimal(5))
        Assert.AreEqual(pairInfo.minPrice, new Decimal(1))
        Assert.AreEqual(pairInfo.maxPrice, new Decimal(12000))
        Assert.AreEqual(pairInfo.minAmount, new Decimal(0.1))
        Assert.AreEqual(pairInfo.hidden, 0)
        Assert.AreEqual(pairInfo.fee, new Decimal(0.2))

        let pairs = pairs.Tail
        let (pair, pairInfo) = pairs.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.EUR))
        Assert.AreEqual(pairInfo.decimalPlaces, new Decimal(5))
        Assert.AreEqual(pairInfo.minPrice, new Decimal(0.1))
        Assert.AreEqual(pairInfo.maxPrice, new Decimal(400))
        Assert.AreEqual(pairInfo.minAmount, new Decimal(0.1))
        Assert.AreEqual(pairInfo.hidden, 0)
        Assert.AreEqual(pairInfo.fee, new Decimal(0.2))

    [<Test>]
    member self.getRecentTrades() = 
        let recentTrades = getRecentTradesWithCustomDownloader mockDownloader [(Currency.BTC, Currency.USD); (Currency.LTC, Currency.USD)]
        let (pair, trades) = recentTrades.Head
        Assert.AreEqual(pair, (Currency.BTC, Currency.USD))
        let trade = trades.Head
        Assert.AreEqual(trade.tradeType, "ask")
        Assert.AreEqual(trade.price, new Decimal(87.7))
        Assert.AreEqual(trade.amount, new Decimal(1.12083))
        Assert.AreEqual(trade.tid, 6455773)
        Assert.AreEqual(trade.timestamp, (int64)1375005305)
        let trade = trades.Item(6)
        Assert.AreEqual(trade.tradeType, "bid")
        Assert.AreEqual(trade.price, new Decimal(88))
        Assert.AreEqual(trade.amount, new Decimal(3.39478))
        Assert.AreEqual(trade.tid, 6455540)
        Assert.AreEqual(trade.timestamp, (int64)1375003848)

        let (pair, trades) = recentTrades.Tail.Head
        Assert.AreEqual(pair, (Currency.LTC, Currency.USD))
        let trade = trades.Head
        Assert.AreEqual(trade.tradeType, "ask")
        Assert.AreEqual(trade.price, new Decimal(2.6609))
        Assert.AreEqual(trade.amount, new Decimal(5.34364))
        Assert.AreEqual(trade.tid, 6455770)
        Assert.AreEqual(trade.timestamp, (int64)1375005299)
        let trade = trades.Item(2)
        Assert.AreEqual(trade.tradeType, "bid")
        Assert.AreEqual(trade.price, new Decimal(2.67191))
        Assert.AreEqual(trade.amount, new Decimal(0.848262))
        Assert.AreEqual(trade.tid, 6455644)
        Assert.AreEqual(trade.timestamp, (int64)1375004933)
end