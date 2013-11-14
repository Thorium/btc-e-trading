open TradingFramework.GeneticProgramming
open TradingFramework.PatternRecognitionGP

let main () =
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

    let doubleRng = (fun () -> random.NextDouble())

    let select = selectNormalisePositive doubleRng

    let initialPrograms = [ for _ in 1..100 do yield growPatternRecogniserTree rng maxDepth maxNumberOfChildren chanceOfLeaf ]
        
    let rec evolve i last programs =
        if i = last then
            programs
        else
            let fittestPrograms = [ for _ in 1..10 do yield select programs fitness ]
            
            // Do crossover and mutation
            let children = List.map (fun x -> x) fittestPrograms

            evolve (i + 1) last children

    let program = evolve 0 10 initialPrograms
    let result = fitness program.Head
    ignore

main() |> ignore