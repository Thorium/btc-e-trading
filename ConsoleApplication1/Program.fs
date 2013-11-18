open TradingFramework.GeneticProgramming
open TradingFramework.PatternRecognitionGP

let main () =

    let random = new System.Random()

    let rng x = random.Next(x)

    let opening = [| for _ in 1..1000 do yield float(rng 100) |]
    let closing = [| for _ in 1..1000 do yield float(rng 100) |]
    let high = [| for _ in 21..1020 do yield float(rng 100) |]
    let low = [| for _ in 11..1010 do yield float(rng 100) |]

    let result = TicTacTec.TA.Library.Core.Cdl3StarsInSouthLookback()

    let data = TaLib.Library.PatternRecognition.cdl3StarsInSouth 12 900 opening closing high low

    ignore

(*
    let maxDepth = 2
    let maxNumberOfChildren = 2
    let chanceOfLeaf = 3 // 1 in 3 chance

    let rec readData fileReader = TradingFramework.BackTesting.readHistoricTickerData fileReader

    let interval = 30

    use sr = new System.IO.StreamReader("ticker.txt")
    let rec reader () =
        if not sr.EndOfStream then
            Some(sr.ReadLine())
        else
            None

    let values = readIntervalData reader readData interval

    let fitness = fitness values

    let random = new System.Random()

    let rng = (fun x -> random.Next(x))

    let select = tournamentSelection rng

    let initialPrograms = [ for _ in 1..100 do yield growPatternRecogniserTree rng maxDepth maxNumberOfChildren chanceOfLeaf ]
        
    let tournamentSize = 15

    let rec evolve i last programs =
        if i = last then
            programs
        else
            let fittestPrograms = [ for _ in 1..10 do yield select programs fitness tournamentSize ]
            
            // Do crossover and mutation
            let children = List.map (fun x -> x) fittestPrograms

            evolve (i + 1) last children

    let program = evolve 0 tournamentSize initialPrograms
    let result = fitness program.Head
    ignore
    *)
main() |> ignore