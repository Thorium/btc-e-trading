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

open MSDN.FSharp.Charting.ChartTypes
open MSDN.FSharp.Charting

open Evolve
open LoadFileLayout

open TradingFramework.PatternRecognitionGP

let zip4 a (b : _ []) (c : _ []) (d : _ []) =
    Array.init (Array.length a) (fun i -> a.[i], b.[i], c.[i], d.[i])

let getTickerList highLowOpenClose =
    zip4 highLowOpenClose.high highLowOpenClose.low highLowOpenClose.opening highLowOpenClose.closing
        |> Array.toList

let chart highLowOpenClose =
    let prices = getTickerList highLowOpenClose

    //Chart.Stock(prices) |> ignore

    let chart = FSharpChart.Candlestick(prices)

    new WindowsFormsHost(Child = new ChartControl(chart))

let appName = "Duckmatt BTC-E Trader"

type Title = class
    inherit Label

    new () as this = {} then
        this.Content <- appName

        this.HorizontalAlignment <- HorizontalAlignment.Left
        this.Margin <- Thickness(0.0)
        this.Padding <- Thickness(0.0, 0.0, 0.0, 5.0)

        this.FontWeight <- FontWeights.Bold
        this.FontSize <- 48.0
        this.FontFamily <- Media.FontFamily("Arial")
        this.Foreground <- Media.SolidColorBrush(Media.Color.FromRgb(byte(60), byte(60), byte(60)))
end

type MainStackPanel = class
    inherit StackPanel

    new () as this = {} then
        this.Margin <- Thickness(20.0, 10.0, 20.0, 0.0)
end

type MainWindow = class
    inherit Window
   
    new () as this = {} then
        this.Title <- appName

        this.MinWidth <- 960.0
        this.MinHeight <- 600.0

        let grid = new System.Windows.Controls.Grid()

        let title = Title()
        grid.Children.Add(title) |> ignore
        Grid.SetRow(title, 0)

        let introduction = Label()
        introduction.Margin <- Thickness(0.0, 0.0, 0.0, 10.0)
        introduction.Padding <- Thickness(0.0)
        let labelText = TextBlock()
        labelText.Text <- "Choose a backtesting file to load. The data from this file will be turned into open high low close data by intervals, an interval of 15 would combine every 15 records from the file into single open high low close records. If the backtesting file fails to load it is in the wrong format."
        labelText.TextWrapping <- TextWrapping.Wrap
        labelText.FontFamily <- Media.FontFamily("Times New Roman")
        labelText.Foreground <- Media.SolidColorBrush(Media.Color.FromRgb(byte(60), byte(60), byte(60)))
        labelText.FontSize <- 14.0
        introduction.Content <- labelText
        grid.Children.Add(introduction) |> ignore
        Grid.SetRow(introduction, 1)

        let loading = fun _ -> ()

        let loaded = fun values -> ()

        let loadFileLayout = LoadFileLayout(loading, loaded)
        grid.Children.Add(loadFileLayout) |> ignore
        Grid.SetRow(loadFileLayout, 2)

        loadFileLayout.LoadedFile.Add (fun highLowOpenClose -> 
            let chart = chart highLowOpenClose
            grid.Children.Add(chart) |> ignore
            chart.MinHeight <- 400.0
            chart.MinWidth <- 500.0
            Grid.SetRow(chart, 3))
      
        List.iter (fun _ ->
            let rowdef = new RowDefinition()
            rowdef.Height <- GridLength.Auto
            grid.RowDefinitions.Add(rowdef) |> ignore) [0..4]

        let panel = MainStackPanel()

        panel.Children.Add(grid) |> ignore

        this.Content <- panel
end