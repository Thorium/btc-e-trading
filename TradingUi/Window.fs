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
 
module Window =
 
    open System
    open System.Windows
    open System.Windows.Input
    open System.Windows.Controls
    open System.Windows.Forms.Integration
 
    open Evolve
    open LoadFileLayout
 
    open TradingGraph.GraphControl
 
    open TradingFramework.PatternRecognitionGP
 
    let zip4 a (b : _ []) (c : _ []) (d : _ []) =
        Array.init (Array.length a) (fun i -> a.[i], b.[i], c.[i], d.[i])
 
    let getTickerList highLowOpenClose =
        zip4 highLowOpenClose.high highLowOpenClose.low highLowOpenClose.opening highLowOpenClose.closing

    let readBacktestingData (backtestingFile: string) interval =
        let rec readData fileReader = TradingFramework.BackTesting.readHistoricTickerData fileReader

        use sr = new System.IO.StreamReader(backtestingFile)
        let rec reader () =
            if not sr.EndOfStream then
                Some(sr.ReadLine())
            else
                None

        readIntervalData reader readData interval

    open TradingGraph.Scrollbar
    open TradingGraph.IGraph
    open TradingGraph.LineGraph
    open TradingGraph.CandleStickGraph
    open TradingGraph.IMovementListener
    open TradingGraph.KineticScroller
 
    type MainWindow = class
        inherit Window
   
        new () as this = {} then
            this.Title <- "Duckmatt BTC-E Trader"
 
            this.MinWidth <- 960.0
            this.MinHeight <- 600.0

            let records = readBacktestingData "ticker.txt" 30

            let simpleMovingAverage = TaLib.Library.MovingAverage.Sma

            let lowMovingAverage = TaLib.Library.Overlap.movingAverage records.low TaLib.Library.taIntegerDefault simpleMovingAverage

            let highMovingAverage = TaLib.Library.Overlap.movingAverage records.high TaLib.Library.taIntegerDefault simpleMovingAverage

            let records = getTickerList records

            let graphControl = new GraphControl(Scrollbar(), HighLowOpenCloseGraph(records))

            graphControl.AddMovementListener <| KineticScroller()

            match lowMovingAverage with
            | TaLib.Library.Success(movingAverage, offset, lastRecord) -> 
                let lineGraph = LineGraph(movingAverage) :> IGraph
                lineGraph.Offset <- offset
                lineGraph.LastRecord <- lastRecord - 1
                graphControl.AddGraph(lineGraph)
            | TaLib.Library.Error(_) -> ()

            match highMovingAverage with
            | TaLib.Library.Success(movingAverage, offset, lastRecord) -> 
                let lineGraph = LineGraph(movingAverage) :> IGraph
                lineGraph.Offset <- offset
                lineGraph.LastRecord <- lastRecord - 1
                graphControl.AddGraph(lineGraph)
            | TaLib.Library.Error(_) -> ()
            
            this.Content <- new WindowsFormsHost(Child = graphControl)
    end