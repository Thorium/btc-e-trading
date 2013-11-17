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

module TestPatternRecognitionGP

open NUnit.Framework

open TradingFramework.GeneticProgramming
open TradingFramework.PatternRecognitionGP

[<TestFixture>]
type TestPatternRecognitionGeneticProgramming() = class
    [<Test>]
    member self.evaluateActions() = 
        Assert.AreEqual(-7, evaluateActions [(Buy, decimal(7))] <| decimal(0))

        Assert.AreEqual(7, evaluateActions [(Sell, decimal(7))] <| decimal(0))

        let actions = [(Buy, decimal(4));(Buy, decimal(10));(Sell, decimal(14));(Buy, decimal(14));(Sell, decimal(20))]
        Assert.AreEqual(6, evaluateActions actions <| decimal(0))

        Assert.AreEqual(16, evaluateActions actions <| decimal(10))

    [<Test>]
    member self.evaluateTree() = 
        Assert.Fail()

    [<Test>]
    member self.readIntervalData() =
        let quote x : BtceApiFramework.PublicBtceApi.Quote = 
            {
                high = decimal(0)
                low = decimal(0)
                average = decimal(0)
                tradingVolume = decimal(0)
                tradingVolumeInCurrency = decimal(0)
                lastTransactionPrice = decimal(0)
                buy = decimal(x)
                sell = decimal(0)
                updated = int64(0)
            }

        let pair = (BtceApiFramework.Currency.Currency.BTC, BtceApiFramework.Currency.Currency.USD)

        let records = [
            [pair, quote 1]
            [pair, quote 2]
            [pair, quote 3]

            [pair, quote 1]
            [pair, quote 1]
            [pair, quote 1]

            [pair, quote 3]
            [pair, quote 4]
            [pair, quote 1]
            
            [pair, quote 10]
        ]

        let readData _ = seq { for record in records do yield record }

        let interval = 3

        let data = readIntervalData () readData interval

        System.Console.WriteLine(data.opening.ToString())
        System.Console.WriteLine(data.closing)
        System.Console.WriteLine(data.high)
        System.Console.WriteLine(data.low)

        Assert.AreEqual([| 1;1;3 |], data.opening)
        Assert.AreEqual([| 3;1;1 |], data.closing)
        Assert.AreEqual([| 3;1;4 |], data.high)
        Assert.AreEqual([| 1;1;1 |], data.low)

    [<Test>]
    member self.evaluateProgramAgainstIntervalData() =
        Assert.Fail()

    [<Test>]
    member self.fitness() =
        Assert.Fail()

    [<Test>]
    member self.mutateValue() =
        let result = mutateValue (fun x -> if x = 2 then 0 else 2) 92 5 100 -100
        Assert.AreEqual(94, result)

        let result = mutateValue (fun x -> if x = 2 then 0 else x) 97 5 100 -100
        Assert.AreEqual(100, result)

        let result = mutateValue (fun x -> if x = 2 then 1 else 2) -92 5 100 -100
        Assert.AreEqual(-94, result)

        let result = mutateValue (fun x -> if x = 2 then 1 else x) -97 5 100 -100
        Assert.AreEqual(-100, result)

        let result = mutateValue (fun x -> if x = 2 then 1 else 2) 3 5 100 0
        Assert.AreEqual(1, result)

        let result = mutateValue (fun x -> if x = 2 then 1 else x) 3 5 100 0
        Assert.AreEqual(0, result)

    [<Test>]
    member self.mutateFunctionArguments() =
        Assert.Fail()

    [<Test>]
    member self.generateRandomFunc() =
        Assert.Fail()
end