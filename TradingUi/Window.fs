module Window

open System.Windows
open System.Windows.Input
open System.Windows.Controls

open Evolve

type Action = delegate of unit -> unit

let runOnUiThread func =
    let action = Action(fun () -> func())
    Application.Current.Dispatcher.Invoke(action) |> ignore

let loadData loading loaded complete _ =
    let dialog = Microsoft.Win32.OpenFileDialog()
    let result = dialog.ShowDialog()
    if result.HasValue && result.Value then
        async { 
            let interval = 15
            let values = readBacktestingData dialog.FileName interval
            runOnUiThread (fun _ -> loaded dialog.FileName)
            testEvolve values
            runOnUiThread (fun _ -> complete dialog.FileName)
        } |> Async.Start |> ignore
        loading dialog.FileName

type MainWindow = class
   inherit Window
   
   new () as this = {} then
      this.Title <- "Trading Application"

      let grid = new Grid()

      let label = Label()
      grid.Children.Add(label) |> ignore
      Grid.SetRow(label, 0)
      Grid.SetColumn(label, 1)

      let button = Button()
      button.Content <- "Load Backtesting File"

      let loaded filename = label.Content <- "Loaded " + filename

      let complete _ = label.Content <- "Evolved data"

      let loadData = loadData (fun filename -> label.Content <- "Loading " + filename) loaded complete

      button.Click.Add loadData

      this.Content <- grid
      
      let addRowDef i =
         let rowdef = new RowDefinition()
         rowdef.Height <- GridLength.Auto
         grid.RowDefinitions.Add(rowdef) |> ignore

      let addColDef i =
         let coldef = new ColumnDefinition()
         coldef.Width <- GridLength.Auto
         grid.ColumnDefinitions.Add(coldef) |> ignore

      List.iter addRowDef [0..2]
      List.iter addColDef [0..2]

      grid.Children.Add(button) |> ignore
      Grid.SetRow(button, 0)
      Grid.SetColumn(button, 0)

      this.Content <- grid
end