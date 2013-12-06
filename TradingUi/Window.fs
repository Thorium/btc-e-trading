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
 
module Window
 
open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Forms.Integration
 
open Evolve
open LoadFileLayout
 
open TradingFramework.PatternRecognitionGP
 
let zip4 a (b : _ []) (c : _ []) (d : _ []) =
    Array.init (Array.length a) (fun i -> a.[i], b.[i], c.[i], d.[i])
 
let getTickerList highLowOpenClose =
    zip4 highLowOpenClose.high highLowOpenClose.low highLowOpenClose.opening highLowOpenClose.closing
        |> Array.toList
 
open Graph
 
type MainWindow = class
    inherit Window
   
    new () as this = {} then
        this.Title <- "Duckmatt BTC-E Trader"
 
        this.MinWidth <- 960.0
        this.MinHeight <- 600.0

        let records = [
            (95.0,66.0,90.0,90.0)
            (166.0,87.0,145.0,87.0)
            (128.0,85.0,123.0,125.0)
        ]

        this.Content <- new WindowsFormsHost(Child = new CoinGraph(records))
end