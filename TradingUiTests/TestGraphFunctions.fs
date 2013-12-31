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

open TradingGraph.GraphFunctions

[<TestFixture>]
type TestGraph() = class
    [<Test>]
    member self.getHighestHighAndLowestLowFromRecords() = 
        let records = [|
            (800.0, 100.0, 400.0, 400.0)
            (654.0, 120.0, 400.0, 400.0)
            (755.0, 140.0, 400.0, 400.0)
            (976.0, 160.0, 400.0, 400.0)
            (890.0, 180.0, 400.0, 400.0)
        |]
        let result = foldRecordsToHighLow highAndLowRecordFolder records
        Assert.AreEqual((976.0, 100.0), result)

    [<Test>]
    member self.getHighestHighAndLowestLowFromRecordsWithNegativeValues() = 
        let records = [|
            (-100.0, -976.0, -400.0, -400.0)
            (-120.0, -800.0, -400.0, -400.0)
            (-170.0, -600.0, -400.0, -400.0)
            (-377.0, -976.0, -400.0, -400.0)
            (-450.0, -976.0, -400.0, -400.0)
        |]
        let result = foldRecordsToHighLow highAndLowRecordFolder records
        Assert.AreEqual((-100.0, -976.0), result)

    [<Test>]
    member self.getHighestHighAndLowestLow() = 
        let records = [| 800.0; 654.0; 755.0; 976.0; 890.0 |]
        let result = foldRecordsToHighLow highAndLowFolder records
        Assert.AreEqual((976.0, 654.0), result)

    [<Test>]
    member self.getHighestHighAndLowestLowWithNegativeValues() = 
        let records = [| -800.0; -654.0; -755.0; -976.0; -890.0 |]
        let result = foldRecordsToHighLow highAndLowFolder records
        Assert.AreEqual((-654.0, -976.0), result)

    [<Test>]
    member self.getRoundedValuesBetween() = 
        Assert.Fail()

    [<Test>]
    member self.getNumberOfRecordsCanBeDisplayed() = 
        let widthOfView, recordWidth, recordLeftMargin = 400, 40, 0

        let actualRecords = getNumberOfRecordsCanBeDisplayed widthOfView recordWidth recordLeftMargin
        
        Assert.AreEqual(10, actualRecords)

        let widthOfView, recordWidth, recordLeftMargin = 400, 10, 9
        
        let actualRecords = getNumberOfRecordsCanBeDisplayed widthOfView recordWidth recordLeftMargin

        Assert.AreEqual(21, actualRecords)

    [<Test>]
    member self.mapXCoordinateToRecordNumber() = 
        let x, recordWidth, recordLeftMargin, leftMostRecord = 140.0, 1, 0, 0

        let actual = mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(140, actual)

        let x, recordWidth, recordLeftMargin, leftMostRecord = 140.0, 1, 1, 0

        let actual = mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(70, actual)

        let x, recordWidth, recordLeftMargin, leftMostRecord = 140.0, 1, 1, 130

        let actual = mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(200, actual)

        let x, recordWidth, recordLeftMargin, leftMostRecord = 145.0, 10, 10, 10

        let actual = mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(17, actual)

    [<Test>]
    member self.mapRecordNumberToXCoordinate() = 
        let recordNumber, recordWidth, recordLeftMargin, leftMostRecord = 10, 10, 10, 10

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(0, actual)

        let recordNumber, recordWidth, recordLeftMargin, leftMostRecord = 15, 10, 10, 10

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(5 * 20, actual)

        let recordNumber, recordWidth, recordLeftMargin, leftMostRecord = 15, 10, 0, 10

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(50, actual)

        let recordNumber, recordWidth, recordLeftMargin, leftMostRecord = 15, 5, 0, 10

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(25, actual)

    [<Test>]
    member self.mapXCoordinateFunctionsInverse() = 
        let x, recordWidth, recordLeftMargin, leftMostRecord = 140.0, 1, 0, 0

        let recordNumber = mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(140, actual)

        let x, recordWidth, recordLeftMargin, leftMostRecord = 147.0, 10, 10, 0

        let recordNumber = (mapXCoordinateToRecordNumber x recordWidth recordLeftMargin leftMostRecord) + 1

        let actual = mapRecordNumberToXCoordinate recordNumber recordWidth recordLeftMargin leftMostRecord

        Assert.AreEqual(160, actual)

    [<Test>]
    member self.mapValueToYCoordinate() = 
        let heightOfView, highestHigh, lowestLow, gap, value = 1000, 100.0f, 90.0f, 0.0f, 98.5f

        let actual = mapValueToYCoordinate heightOfView (highestHigh, lowestLow) gap value

        Assert.AreEqual(150.0f, actual)

        let heightOfView, highestHigh, lowestLow, gap, value = 1000, 100.0f, 90.0f, 0.0f, 100.0f

        let actual = mapValueToYCoordinate heightOfView (highestHigh, lowestLow) gap value

        Assert.AreEqual(0.0f, actual)

        let heightOfView, highestHigh, lowestLow, gap, value = 1000, 100.0f, 90.0f, 0.0f, 90.0f

        let actual = mapValueToYCoordinate heightOfView (highestHigh, lowestLow) gap value

        Assert.AreEqual(1000.0f, actual)

    [<Test>]
    member self.mapHeight() = 
        Assert.Fail()

    [<Test>]
    member self.getCandleStickLine() = 
        Assert.Fail()

    [<Test>]
    member self.getCandleStick() = 
        Assert.Fail()

    [<Test>]
    member self.paintCandleStick() = 
        Assert.Fail()

    [<Test>]
    member self.paintCoordinates() = 
        Assert.Fail()

    [<Test>]
    member self.paintYAxisLabel() = 
        Assert.Fail()

    [<Test>]
    member self.paintYAxis() = 
        Assert.Fail()
end