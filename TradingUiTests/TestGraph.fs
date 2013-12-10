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

open Graph

[<TestFixture>]
type TestGraph() = class
    [<Test>]
    member self.getHighestHighAndLowestLow() = 
        let records = [|
            (800.0, 100.0, 400.0, 400.0)
            (654.0, 120.0, 400.0, 400.0)
            (755.0, 140.0, 400.0, 400.0)
            (976.0, 160.0, 400.0, 400.0)
            (890.0, 180.0, 400.0, 400.0)
        |]
        let result = getHighestHighAndLowestLow records
        Assert.AreEqual((976.0, 100.0), result)

    [<Test>]
    member self.getHighestHighAndLowestLowWithNegativeValues() = 
        let records = [|
            (-100.0, -976.0, -400.0, -400.0)
            (-120.0, -800.0, -400.0, -400.0)
            (-170.0, -600.0, -400.0, -400.0)
            (-377.0, -976.0, -400.0, -400.0)
            (-450.0, -976.0, -400.0, -400.0)
        |]
        let result = getHighestHighAndLowestLow records
        Assert.AreEqual((-100.0, -976.0), result)
end