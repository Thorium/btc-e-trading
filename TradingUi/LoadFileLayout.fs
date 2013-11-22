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

open TradingFramework.PatternRecognitionGP

type Action = delegate of unit -> unit

let runOnUiThread func =
    let action = Action(fun () -> func())
    Application.Current.Dispatcher.Invoke(action) |> ignore

type LoadBacktestingResult =
| Loaded of OpenHighLowClose
| FailedToLoad

let getFile () =
    let dialog = Microsoft.Win32.OpenFileDialog()
    let result = dialog.ShowDialog()
    if result.HasValue && result.Value then
        Some(dialog.FileName)
    else
        None
        
let loadData filename interval loading complete =
    async { 
        runOnUiThread (fun _ -> loading filename)
        let values = try
                        Loaded(readBacktestingData filename interval)
                        with
                        | :? _ -> FailedToLoad
        runOnUiThread (fun _ -> complete filename values)
    } |> Async.Start |> ignore

type LoadFileLayout(loadingCallback, loadedCallback) as this =
    inherit Grid()

    let loadedFile = Event<OpenHighLowClose>()

    do 
        let rows, columns = 3, 3
        List.iter (fun _ ->
            let columnDefinition = ColumnDefinition()
            columnDefinition.Width <- GridLength.Auto
            this.ColumnDefinitions.Add(columnDefinition) |> ignore) [0..columns]

        List.iter (fun _ ->
            let rowDefinition = RowDefinition()
            rowDefinition.Height <- GridLength.Auto
            this.RowDefinitions.Add(rowDefinition) |> ignore) [0..rows]

    let fileErrorLabel = Label()
    do
        fileErrorLabel.Foreground <- Media.Brushes.Red
        fileErrorLabel.Content <- "Failed to load file."
        Grid.SetRow(fileErrorLabel, 2)
        Grid.SetColumn(fileErrorLabel, 3)

    let showFileErrorLabel() =
        this.Children.Add(fileErrorLabel) |> ignore

    let hideFileErrorLabel() =
        this.Children.Remove(fileErrorLabel)

    let loadBacktestingDataButton = Button()
    do
        loadBacktestingDataButton.Content <- "Load Backtesting Data"
        loadBacktestingDataButton.IsEnabled <- false
        loadBacktestingDataButton.Margin <- Thickness(0.0, 5.0, 0.0, 5.0)
        this.Children.Add(loadBacktestingDataButton) |> ignore
        Grid.SetRow(loadBacktestingDataButton, 3)

    let intervalText = IntegerTextBox(60 * 24)
    do 
        intervalText.MinWidth <- 100.0
        intervalText.Margin <- Thickness(5.0, 5.0, 0.0, 5.0)
        this.Children.Add(intervalText) |> ignore
        Grid.SetRow(intervalText, 1)
        Grid.SetColumn(intervalText, 1)

    let backtestingFileTextbox = TextBox()
    do
        backtestingFileTextbox.IsReadOnly <- true
        backtestingFileTextbox.Margin <- Thickness(5.0, 0.0, 0.0, 5.0)
        backtestingFileTextbox.MinWidth <- 350.0
        this.Children.Add(backtestingFileTextbox) |> ignore
        Grid.SetRow(backtestingFileTextbox, 2)
        Grid.SetColumn(backtestingFileTextbox, 1)
        
    let backtestingFileButton = Button()
    do
        backtestingFileButton.Content <- "Backtesting File"
        backtestingFileButton.Margin <- Thickness(0.0, 0.0, 0.0, 5.0)
        this.Children.Add(backtestingFileButton) |> ignore
        Grid.SetRow(backtestingFileButton, 2)

    let disableAll() =
        backtestingFileButton.IsEnabled <- false
        backtestingFileTextbox.IsEnabled <- false
        intervalText.IsEnabled <- false

    let enableAll() =
        backtestingFileButton.IsEnabled <- true
        backtestingFileTextbox.IsEnabled <- true
        intervalText.IsEnabled <- true
        
    let intervalChanged (_, interval) =
        match interval with
        | Some(x) when x > 0 && backtestingFileTextbox.Text.Length > 0 -> 
            loadBacktestingDataButton.IsEnabled <- true
        | _ -> loadBacktestingDataButton.IsEnabled <- false

    let getFile _ =
        match getFile() with
        | Some(filename) when intervalText.Text.Length > 0 -> 
            hideFileErrorLabel()
            backtestingFileTextbox.Text <- filename
            loadBacktestingDataButton.IsEnabled <- true
        | Some(filename) -> 
            hideFileErrorLabel()
            backtestingFileTextbox.Text <- filename
        | None -> ()

    do
        backtestingFileButton.Click.Add getFile

        let loading _ = 
            hideFileErrorLabel()
            disableAll()

        let completed _ = function
        | Loaded(values) -> loadedFile.Trigger(values)
        | FailedToLoad -> 
            enableAll()
            showFileErrorLabel()
            loadBacktestingDataButton.IsEnabled <- true

        loadBacktestingDataButton.Click.Add <| (fun _ -> 
            loadBacktestingDataButton.IsEnabled <- false
            loadData backtestingFileTextbox.Text (intervalText.GetInteger()) loading completed)

        intervalText.IntegerChanged.Add intervalChanged

        this.createIntervalLabel(intervalText)

    [<CLIEvent>]
    member this.LoadedFile = loadedFile.Publish

    member private this.createIntervalLabel(target) =
        let intervalLabel = TargetLabel()
        intervalLabel.Content <- "Interval: "
        intervalLabel.Margin <- Thickness(0.0, 5.0, 0.0, 5.0)
        intervalLabel.Padding <- Thickness(0.0)
        intervalLabel.Target <- target
        this.Children.Add(intervalLabel) |> ignore
        Grid.SetRow(intervalLabel, 1)