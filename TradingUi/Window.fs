module Window

open System.Windows
open System.Windows.Input
open System.Windows.Controls

open Evolve

type Action = delegate of unit -> unit

let runOnUiThread func =
    let action = Action(fun () -> func())
    Application.Current.Dispatcher.Invoke(action) |> ignore

type Title = class
    inherit Label

    new () as this = {} then
        this.Content <- "Duckmatt BTC-E Trader"

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

let loadData loading loaded _ =
    let dialog = Microsoft.Win32.OpenFileDialog()
    let result = dialog.ShowDialog()
    if result.HasValue && result.Value then
        async { 
            runOnUiThread (fun _ -> loading dialog.FileName)
            let interval = 15
            let values = readBacktestingData dialog.FileName interval
            runOnUiThread (fun _ -> loaded dialog.FileName values)
        } |> Async.Start |> ignore
        loading dialog.FileName

type LoadFileLayout = class
    inherit Grid

    new (loadingCallback, loadedCallback)  as this = {} then
        let rowDefinition = RowDefinition()
        rowDefinition.Height <- GridLength.Auto
        this.RowDefinitions.Add(rowDefinition) |> ignore

        // Two columns
        List.iter (fun _ ->
            let columnDefinition = ColumnDefinition()
            columnDefinition.Width <- GridLength.Auto
            this.ColumnDefinitions.Add(columnDefinition) |> ignore) [0..1]

        let label = Label()
        this.Children.Add(label) |> ignore
        Grid.SetRow(label, 1)
        Grid.SetColumn(label, 1)

        let button = Button()
        button.Content <- "Load Backtesting File"

        let loading filename =
            label.Content <- "Loading " + filename
            button.IsEnabled <- false
            loadingCallback()

        let loaded filename values = 
            label.Content <- "Loaded " + filename
            button.IsEnabled <- true
            loadedCallback values

        button.Click.Add <| loadData loading loaded

        this.Children.Add(button) |> ignore
        Grid.SetRow(button, 1)
end

type MainWindow = class
    inherit Window
   
    new () as this = {} then
        this.Title <- "Trading Application"

        this.MinWidth <- 960.0
        this.MinHeight <- 600.0

        let grid = new Grid()

        let title = Title()
        grid.Children.Add(title) |> ignore
        Grid.SetRow(title, 0)

        let introduction = Label()
        introduction.Content <- "Introduction"
        grid.Children.Add(introduction) |> ignore
        Grid.SetRow(introduction, 1)

        let loading = fun _ -> ()

        let loaded = fun values -> ()

        let loadFileLayout = LoadFileLayout(loading, loaded)
        grid.Children.Add(loadFileLayout) |> ignore
        Grid.SetRow(loadFileLayout, 2)
      
        List.iter (fun _ ->
            let rowdef = new RowDefinition()
            rowdef.Height <- GridLength.Auto
            grid.RowDefinitions.Add(rowdef) |> ignore) [0..2]

        let panel = MainStackPanel()

        panel.Children.Add(grid) |> ignore

        this.Content <- panel
end