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

module Evolve =

    open TradingFramework.GeneticProgramming
    open TradingFramework.PatternRecognitionGP

    let readBacktestingData (backtestingFile: string) interval =
        let rec readData fileReader = TradingFramework.BackTesting.readHistoricTickerData fileReader

        use sr = new System.IO.StreamReader(backtestingFile)
        let rec reader () =
            if not sr.EndOfStream then
                Some(sr.ReadLine())
            else
                None

        readIntervalData reader readData interval
    
    let copyNodeValue nodeValue data =
        {
            BranchNumber = nodeValue.BranchNumber
            LeafNumber = nodeValue.LeafNumber
            NumberOfBranches = nodeValue.NumberOfBranches
            NumberOfLeafs = nodeValue.NumberOfLeafs
            Data = data
        }

    let testEvolve values =
        let maxDepth = 3
        let maxNumberOfChildren = 3
        let chanceOfLeaf = 10 // 1 in 3 chance

        let fitness = fitness values

        let random = new System.Random()

        let rng = (fun x -> random.Next(x))

        let select = tournamentSelectionPopulationWithFitness rng

        let populationSize = 100

        let initialPrograms = [ for _ in 1..populationSize do yield growPatternRecogniserTree rng maxDepth maxNumberOfChildren chanceOfLeaf ]
        
        let tournamentSize = 15

        let chanceOfMutation = 30 // 1 in 30 chance

        let mutate = function 
        | Leaf(nodeValue) -> Leaf(copyNodeValue nodeValue <| generateRandomAction rng)
        | Branch(nodeValue, children) -> 
            let nodeValue = if rng chanceOfMutation = 1 then 
                                nodeValue
                            else 
                                nodeValue
            Branch(copyNodeValue nodeValue nodeValue.Data, children)

        // Only crossover on two branch nodes.
        let crossover lhs rhs selectNode mutate =
            match lhs.root with
            | Branch(_, _) -> 
                match rhs.root with
                | Branch(_, _) -> crossover lhs rhs selectNode mutate
                | Leaf(_) -> lhs, rhs
            | Leaf(_) -> lhs, rhs

        let rec evolve i limit programs =
            if i = limit then
                programs
            else
                let programsWithFitness = getPopulationWithFitness programs fitness

                let children = [ 
                    for _ in 1..populationSize / 2 do 
                        let lhs = select programsWithFitness tournamentSize
                        let rhs = select programsWithFitness tournamentSize
                        let chanceOfLeaf = 10
                        let selectNode = selectNode chanceOfLeaf rng
                        let lhs, rhs = crossover lhs rhs selectNode mutate
                        yield lhs
                        yield rhs
                ]

                evolve (i + 1) limit children

        let evolutionIterationLimit = 100

        let program = evolve 0 evolutionIterationLimit initialPrograms
        let result = fitness program.Head
        ()