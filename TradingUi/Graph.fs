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
 
namespace TradingUi

module Graph =

    open System.Drawing
    open System.Drawing.Drawing2D

    open GraphFunctions

    type IGraph =
       abstract member Draw : 
            graphics:Graphics -> 
            leftMostRecord:int ->
            width:int * height:int -> 
            candleWidth:int ->
            candleLeftMargin:int -> 
            highLabel:float32 * lowLabel:float32 ->
            gap:float32 ->
            unit

        abstract member RecordWidth : unit -> int

        abstract member RecordMargin : unit -> int

        abstract member Zoom : int -> unit

        abstract member HighAndLow : leftMostRecord:int -> numberOfRecordsDisplayed:int -> float * float

        abstract Offset : int with get, set

        abstract LastRecord : int with get, set

        abstract member NumberOfRecords : unit -> int

    type HighLowOpenClose = (float * float * float * float) array

    type HighLowOpenCloseGraph(records:HighLowOpenClose) =
        let mutable offset = 0
        let mutable lastRecord = records.Length - 1
        let mutable recordWidth = 7

        let getHighestHighAndLowestLow (currentHigh, currentLow) (high, low, _, _) =
            let high = if high > currentHigh then high else currentHigh
            let low = if low < currentLow then low else currentLow
            high, low

        let getHighestHighAndLowestLow = foldRecordsToHighLow getHighestHighAndLowestLow

        interface IGraph with 
            member this.Draw (graphics:Graphics) leftMostRecord (width, height) candleWidth candleLeftMargin (highLabel, lowLabel) gap =
                let lastRecord = leftMostRecord + (getNumberOfRecordsCanBeDisplayed width candleWidth candleLeftMargin) - 1

                let lastRecord = if lastRecord >= records.Length then records.Length - 1 else lastRecord

                let records = records.[leftMostRecord..lastRecord]

                let high, low = getHighestHighAndLowestLow records

                paintCandleSticks graphics (highLabel, lowLabel) gap records candleWidth candleLeftMargin leftMostRecord height

            member this.RecordWidth () = recordWidth

            member this.RecordMargin () = 3

            member this.Zoom scale = 
                if recordWidth + scale > 0 && recordWidth + scale < 20 then
                    recordWidth <- recordWidth + scale

            member this.HighAndLow leftMostRecord numberOfRecordsDisplayed =
                let lastRecord = leftMostRecord + numberOfRecordsDisplayed - 1
                let lastRecord = if lastRecord >= records.Length then records.Length - 1 else lastRecord

                getHighestHighAndLowestLow records.[leftMostRecord..lastRecord]

            member this.Offset
                with get () = offset
                and set (value) = offset <- value

            member this.LastRecord
                with get () = lastRecord
                and set (value) = lastRecord <- value

            member this.NumberOfRecords () = records.Length
                
    type LineGraph(records:float array) =
        let mutable offset = 0
        let mutable lastRecord = records.Length - 1

        interface IGraph with 
            member this.Draw (graphics:Graphics) leftMostRecord (width, height) candleWidth candleLeftMargin (highLabel, lowLabel) gap =
                let numberOfRecordsDisplayed = getNumberOfRecordsCanBeDisplayed width candleWidth candleLeftMargin

                let lastRecord = 
                    if leftMostRecord + numberOfRecordsDisplayed > lastRecord then lastRecord
                    else leftMostRecord + numberOfRecordsDisplayed

                let points = Array.mapi (fun i value -> 
                    PointF(float32 (i * (candleWidth + candleLeftMargin)), mapValueToYCoordinate height (highLabel, lowLabel) gap (float32 value))) records.[leftMostRecord..lastRecord]

                use pen = new Pen(Color.White, float32(1))
                graphics.SmoothingMode <- SmoothingMode.AntiAlias
                graphics.DrawLines(pen, points)
                graphics.SmoothingMode <- SmoothingMode.Default

            member this.RecordWidth () = 1

            member this.RecordMargin () = 0

            member this.Zoom scale = ()

            member this.HighAndLow leftMostRecord numberOfRecordsDisplayed =
                let finalRecord = leftMostRecord + numberOfRecordsDisplayed - 1
                let lastRecord = if finalRecord > lastRecord then lastRecord else finalRecord

                let getHighestHighAndLowestLow (currentHigh, currentLow) value =
                    let high = if value > currentHigh then value else currentHigh
                    let low = if value < currentLow then value else currentLow
                    high, low

                foldRecordsToHighLow getHighestHighAndLowestLow records.[leftMostRecord..lastRecord]

            member this.Offset
                with get () = offset
                and set (value) = offset <- value

            member this.LastRecord
                with get () = lastRecord
                and set (value) = lastRecord <- value

            member this.NumberOfRecords () = records.Length