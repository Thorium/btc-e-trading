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

namespace TradingGraph

module CandleStickGraph =

    open System.Drawing
    open System.Drawing.Drawing2D

    open IGraph
    open GraphFunctions

    type HighLowOpenClose = (float * float * float * float) array

    type HighLowOpenCloseGraph(records:HighLowOpenClose) =
        let mutable offset = 0
        let mutable lastRecord = records.Length - 1
        let mutable recordWidth = 7

        let getHighestHighAndLowestLow = foldRecordsToHighLow highAndLowRecordFolder

        interface IGraph with 
            member this.Draw (graphics:Graphics) leftMostRecord (width, height) candleWidth candleLeftMargin (highLabel, lowLabel) gap marginTop =
                if lastRecord >= leftMostRecord then
                    let lastRecord = leftMostRecord + (getNumberOfRecordsCanBeDisplayed width candleWidth candleLeftMargin) - 1

                    let lastRecord = if lastRecord >= records.Length then records.Length - 1 else lastRecord

                    let records = records.[leftMostRecord..lastRecord]

                    let high, low = getHighestHighAndLowestLow records

                    paintCandleSticks graphics (highLabel, lowLabel) gap records candleWidth candleLeftMargin leftMostRecord height marginTop

            member this.RecordWidth () = recordWidth

            member this.RecordMargin () = 3

            member this.Zoom scale = 
                if recordWidth + scale > 0 && recordWidth + scale < 20 then
                    recordWidth <- recordWidth + scale

            member this.HighAndLow leftMostRecord numberOfRecordsDisplayed =
                let lastRecord = leftMostRecord + numberOfRecordsDisplayed
                let lastRecord = if lastRecord >= records.Length then records.Length - 1 else lastRecord

                if lastRecord >= leftMostRecord then
                    Some(getHighestHighAndLowestLow records.[leftMostRecord..lastRecord])
                else
                    None

            member this.Offset
                with get () = offset
                and set (value) = offset <- value

            member this.LastRecord
                with get () = lastRecord
                and set (value) = lastRecord <- value

            member this.NumberOfRecords () = records.Length