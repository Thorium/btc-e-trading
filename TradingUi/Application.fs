module TradingUi

open System
open System.Windows

open Window

type TradingApplication = class
   inherit Application
   
   new () = {}
   
   override this.OnStartup args =
      base.OnStartup(args)
      
      MainWindow().Show()      
end

[<STAThread>]
do 
    TradingApplication().Run() |> ignore