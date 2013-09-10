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
    member self.generateImmediateValues() = 
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
    member self.readBacktestingData() = 
        let tickerData = 
            "2013-08-06 08:09:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.466,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:10:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.467,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:11:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.468,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n" +
            "\n2013-08-06 08:13:02 db error" +
            "\n2013-08-06 08:14:02 " +
            "\n" +
            "\n" +
            "\n" +
            "\n" +
            "\n" +
            "\n2013-08-06 08:20:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.469,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:21:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.470,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:22:02 {\"btc_usd\":{\"high\":96.81,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.471,\"sell\":96.465,\"updated\":1375790940}}"

        let i = ref 0
        let lines = tickerData.Split([|'\n'|])

        let reader () =
            if !i < lines.Length then
                let j = !i
                i.Value <- !i + 1
                Some(lines.[j])
            else
                None

        let values = new System.Collections.Generic.List<PublicBtceApi.Quote>()

        BackTesting.readHistoricTickerData reader (fun x -> 
            let (_, quote) = x.Head
            values.Add(quote) |> ignore)

        Assert.AreEqual(lines.Length, values.Count)

        Assert.AreEqual(new Decimal(10), values.[0].buy)
        Assert.AreEqual(new Decimal(10), values.[1].buy)
        Assert.AreEqual(new Decimal(10), values.[2].buy)
        Assert.AreEqual(new Decimal(10), values.[3].buy)
        Assert.AreEqual(new Decimal(10), values.[4].buy)
        Assert.AreEqual(new Decimal(10), values.[5].buy)
        Assert.AreEqual(new Decimal(10), values.[6].buy)
        Assert.AreEqual(new Decimal(10), values.[7].buy)
        Assert.AreEqual(new Decimal(10), values.[8].buy)
        Assert.AreEqual(new Decimal(10), values.[9].buy)
        Assert.AreEqual(new Decimal(10), values.[10].buy)
        Assert.AreEqual(new Decimal(10), values.[11].buy)
        Assert.AreEqual(new Decimal(10), values.[12].buy)
        Assert.AreEqual(new Decimal(10), values.[13].buy)
end