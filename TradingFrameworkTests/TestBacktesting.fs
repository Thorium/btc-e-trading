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
        let generateIntermediateValue = generateIntermediateValue numberOfEmptyPlaces (currency, precedingRecord) (currency, followingRecord)

        let (_, value) = (generateIntermediateValue 1)
        Assert.AreEqual(new Decimal(20), value.high)
        Assert.AreEqual(new Decimal(40), value.buy)
        Assert.AreEqual(new Decimal(10), value.sell)
        Assert.AreEqual(new Decimal(-10), value.average)
        Assert.AreEqual(new Decimal(10), value.tradingVolume)

        let (_, value) = (generateIntermediateValue 2)
        Assert.AreEqual(new Decimal(30), value.high)
        Assert.AreEqual(new Decimal(30), value.buy)
        Assert.AreEqual(new Decimal(10), value.sell)
        Assert.AreEqual(new Decimal(0), value.average)
        Assert.AreEqual(new Decimal(0), value.tradingVolume)

        let (_, value) = (generateIntermediateValue 3)
        Assert.AreEqual(new Decimal(40), value.high)
        Assert.AreEqual(new Decimal(20), value.buy)
        Assert.AreEqual(new Decimal(10), value.sell)
        Assert.AreEqual(new Decimal(10), value.average)
        Assert.AreEqual(new Decimal(-10), value.tradingVolume)

    [<Test>]
    member self.readBacktestingData() = 
        let tickerData = 
            "2013-08-06 08:09:02 {\"btc_usd\":{\"high\":-10,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":97.5,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:10:02 {\"btc_usd\":{\"high\":-9.5,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":97,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:11:02 {\"btc_usd\":{\"high\":-9,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":96.5,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n" +
            "\n2013-08-06 08:13:02 db error" +
            "\n2013-08-06 08:14:02 " +
            "\n" +
            "\n" +
            "\n" +
            "\n" +
            "\n" +
            "\n2013-08-06 08:20:02 {\"btc_usd\":{\"high\":-4.5,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":92,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:21:02 {\"btc_usd\":{\"high\":-4,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":91.5,\"sell\":96.465,\"updated\":1375790940}}" +
            "\n2013-08-06 08:22:02 {\"btc_usd\":{\"high\":-3.5,\"low\":94.857,\"avg\":95.8335,\"vol\":215902.82718,\"vol_cur\":2247.48188,\"last\":96.465,\"buy\":91,\"sell\":96.465,\"updated\":1375790940}}"

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

        Assert.AreEqual(new Decimal(97.5), values.[0].buy)
        Assert.AreEqual(new Decimal(97), values.[1].buy)
        Assert.AreEqual(new Decimal(96.5), values.[2].buy)
        Assert.AreEqual(new Decimal(96), values.[3].buy)
        Assert.AreEqual(new Decimal(95.5), values.[4].buy)
        Assert.AreEqual(new Decimal(95), values.[5].buy)
        Assert.AreEqual(new Decimal(94.5), values.[6].buy)
        Assert.AreEqual(new Decimal(94), values.[7].buy)
        Assert.AreEqual(new Decimal(93.5), values.[8].buy)
        Assert.AreEqual(new Decimal(93), values.[9].buy)
        Assert.AreEqual(new Decimal(92.5), values.[10].buy)
        Assert.AreEqual(new Decimal(92), values.[11].buy)
        Assert.AreEqual(new Decimal(91.5), values.[12].buy)
        Assert.AreEqual(new Decimal(91), values.[13].buy)

        Assert.AreEqual(new Decimal(-10), values.[0].high)
        Assert.AreEqual(new Decimal(-9.5), values.[1].high)
        Assert.AreEqual(new Decimal(-9), values.[2].high)
        Assert.AreEqual(new Decimal(-8.5), values.[3].high)
        Assert.AreEqual(new Decimal(-8), values.[4].high)
        Assert.AreEqual(new Decimal(-7.5), values.[5].high)
        Assert.AreEqual(new Decimal(-7), values.[6].high)
        Assert.AreEqual(new Decimal(-6.5), values.[7].high)
        Assert.AreEqual(new Decimal(-6), values.[8].high)
        Assert.AreEqual(new Decimal(-5.5), values.[9].high)
        Assert.AreEqual(new Decimal(-5), values.[10].high)
        Assert.AreEqual(new Decimal(-4.5), values.[11].high)
        Assert.AreEqual(new Decimal(-4), values.[12].high)
        Assert.AreEqual(new Decimal(-3.5), values.[13].high)
end