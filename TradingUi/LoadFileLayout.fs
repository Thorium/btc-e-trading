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

module LoadFileLayout

open IntegerTextBox

open System.Windows
open System.Windows.Input
open System.Windows.Controls

open TargetLabel
open Evolve

type Action = delegate of unit -> unit

let runOnUiThread func =
    let action = Action(fun () -> func())
    Application.Current.Dispatcher.Invoke(action) |> ignore

type LoadBacktestingResult =
| Loaded of TradingFramework.PatternRecognitionGP.OpenHighLowClose
| FailedToLoad of string

let getFile () =
    let dialog = Microsoft.Win32.OpenFileDialog()
    let result = dialog.ShowDialog()
    if result.HasValue && result.Value then
        Some(dialog.FileName)
    else
        None
        
let loadData loading complete _ =
    let dialog = Microsoft.Win32.OpenFileDialog()
    let result = dialog.ShowDialog()
    if result.HasValue && result.Value then
        async { 
            runOnUiThread (fun _ -> loading dialog.FileName)
            let interval = 15
            let values = try
                           Loaded(readBacktestingData dialog.FileName interval)
                         with
                           | :? _ -> FailedToLoad("")
            runOnUiThread (fun _ -> complete dialog.FileName values)
        } |> Async.Start |> ignore
        loading dialog.FileName

type LoadFileLayout = class
    inherit Grid

    new (loadingCallback, loadedCallback)  as this = {} then
        this.setupGrid()

        let label = this.createBacktestingFileTextBox()

        let backtestingFileButton = this.createBacktestingFileButton()

        let getFile _ =
            match getFile() with
            | Some(filename) -> label.Text <- filename
            | None -> ()

        backtestingFileButton.Click.Add getFile
        label.PreviewMouseDown.Add getFile

        let intervalText = this.createIntervalTextbox()
        
        let intervalChanged (_, interval) =
            match interval with
            | Some(x) when x > 0 -> backtestingFileButton.IsEnabled <- true
            | _ -> backtestingFileButton.IsEnabled <- false

        intervalText.IntegerChanged.Add intervalChanged

        this.createIntervalLabel(intervalText)

        this.createLoadBacktestingDataButton()

    member private this.setupGrid() =
        List.iter (fun _ ->
            let columnDefinition = ColumnDefinition()
            columnDefinition.Width <- GridLength.Auto
            this.ColumnDefinitions.Add(columnDefinition) |> ignore) [0..3]

        List.iter (fun _ ->
            let rowDefinition = RowDefinition()
            rowDefinition.Height <- GridLength.Auto
            this.RowDefinitions.Add(rowDefinition) |> ignore) [0..3]

    member private this.createIntervalLabel(target) =
        let intervalLabel = TargetLabel()
        intervalLabel.Content <- "Interval: "
        intervalLabel.Padding <- Thickness(0.0)
        intervalLabel.Target <- target
        this.Children.Add(intervalLabel) |> ignore
        Grid.SetRow(intervalLabel, 1)

    member private this.createIntervalTextbox() : IntegerTextBox =
        let intervalText = IntegerTextBox(60 * 24)
        intervalText.MinWidth <- 100.0
        this.Children.Add(intervalText) |> ignore
        Grid.SetRow(intervalText, 1)
        Grid.SetColumn(intervalText, 1)
        intervalText

    member private this.createBacktestingFileButton() : Button =
        let button = Button()
        button.Content <- "Backtesting File"
        this.Children.Add(button) |> ignore
        Grid.SetRow(button, 2)
        button

    member private this.createBacktestingFileTextBox() : TextBox =
        let label = TextBox()
        label.IsReadOnly <- true
        this.Children.Add(label) |> ignore
        Grid.SetRow(label, 2)
        Grid.SetColumn(label, 1)
        label

    member private this.createLoadBacktestingDataButton() =
        let loadBacktestingDataButton = Button()
        loadBacktestingDataButton.Content <- "Load Backtesting Data"
        loadBacktestingDataButton.IsEnabled <- false
        this.Children.Add(loadBacktestingDataButton) |> ignore
        Grid.SetRow(loadBacktestingDataButton, 3)
        Grid.SetColumn(loadBacktestingDataButton, 3)
end