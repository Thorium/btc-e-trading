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
        Assert.Fail()

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