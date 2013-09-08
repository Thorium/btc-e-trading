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

module TestBacktesting

open System

open NUnit.Framework

open BtceApiFramework

open BackTesting

[<TestFixture>]
type TestBacktesting() = class

    [<Test>]
    member self.testGeneratingImmediateValues() = 
        let precedingRecord: PublicBtceApi.Quote = {
            high = new Decimal(10);
            low = new Decimal(0);
            average = new Decimal(-20);
            tradingVolume = new Decimal(20);
            tradingVolumeInCurrency = new Decimal(0);
            lastTransactionPrice = new Decimal(0);
            buy = new Decimal(50);
            sell = new Decimal(10);
            updated = (int64)0;
        }

        let followingRecord: PublicBtceApi.Quote = {
            high = new Decimal(50);
            low = new Decimal(0);
            average = new Decimal(20);
            tradingVolume = new Decimal(-20);
            tradingVolumeInCurrency = new Decimal(0);
            lastTransactionPrice = new Decimal(0);
            buy = new Decimal(10);
            sell = new Decimal(10);
            updated = (int64)0;
        }

        let currency = (Currency.Currency.LTC, Currency.Currency.BTC)
        let numberOfEmptyPlaces = 3
        let itermediateRecords = generateIntermediateValues numberOfEmptyPlaces (currency, precedingRecord) (currency, followingRecord)
        let itermediateQuotes = List.map (fun (_, quote) -> quote) itermediateRecords

        Assert.AreEqual(3, itermediateQuotes.Length)

        Assert.AreEqual(new Decimal(20), itermediateQuotes.Head.high)
        Assert.AreEqual(new Decimal(30), itermediateQuotes.Tail.Head.high)
        Assert.AreEqual(new Decimal(40), itermediateQuotes.Tail.Tail.Head.high)
        
        Assert.AreEqual(new Decimal(40), itermediateQuotes.Head.buy)
        Assert.AreEqual(new Decimal(30), itermediateQuotes.Tail.Head.buy)
        Assert.AreEqual(new Decimal(20), itermediateQuotes.Tail.Tail.Head.buy)
        
        Assert.AreEqual(new Decimal(10), itermediateQuotes.Head.sell)
        Assert.AreEqual(new Decimal(10), itermediateQuotes.Tail.Head.sell)
        Assert.AreEqual(new Decimal(10), itermediateQuotes.Tail.Tail.Head.sell)
        
        Assert.AreEqual(new Decimal(-10), itermediateQuotes.Head.average)
        Assert.AreEqual(new Decimal(0), itermediateQuotes.Tail.Head.average)
        Assert.AreEqual(new Decimal(10), itermediateQuotes.Tail.Tail.Head.average)
        
        Assert.AreEqual(new Decimal(10), itermediateQuotes.Head.tradingVolume)
        Assert.AreEqual(new Decimal(0), itermediateQuotes.Tail.Head.tradingVolume)
        Assert.AreEqual(new Decimal(-10), itermediateQuotes.Tail.Tail.Head.tradingVolume)

    [<Test>]
    member self.testnothing() = 
        let values = BackTesting.readHistoricTickerData "ticker.txt"
        for value in values do
            let (x, y) = value.Head
            System.Console.WriteLine(y.buy)
        ()
end