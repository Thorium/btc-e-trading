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

module GeneticProgramming

open System

type NodeData =
    | OverlapFunction of (unit -> int)
    | TimePeriodConstant of Decimal
    | DistanceFromOverlapBuy of Decimal
    | DistanceFromOverlapSell of Decimal 
    | Buy of Decimal
    | Sell of Decimal
(*
    | CycleIndicatorFunction
    | OverlapFunction
    | MomentumFunction
    | PatternRecognitionFunction
    | TimePeriod
*)

type Node = {
    children: Node list
    data: NodeData
}

type EvaluationTree = {
    root: Option<Node>
}

let populate = ignore

let fitness (program: EvaluationTree) : int = 1

let selectWithRandomNumberGenerator (randomNumberGenerator: unit -> double) (population: EvaluationTree list) : EvaluationTree list =
    assert (population.Length > 1)

    let fitness program = double(fitness program)

    let productOfFitness = List.fold (fun product populate -> product * fitness(populate)) 1.0 population
    
    let populationWithProbability = List.map (fun populate -> (populate, fitness(populate) / productOfFitness)) population

    // Descending order
    let populationWithProbability = List.sortBy (fun (populate, probability) -> -probability) populationWithProbability
     
    let randomNumber = randomNumberGenerator()

    assert (randomNumber >= 0.0 && randomNumber < 1.0)

    let rec selectPopulate populationWithProbability accumulator =
        match populationWithProbability with
            | head :: tail when randomNumber < (match head with | (_, probability) -> probability) + accumulator ->  
                let (populate, _) = head
                populate
            | head :: tail ->  
                let (_, probability) = head
                selectPopulate tail (probability + accumulator)
            | [] -> 
                failwith "Unreachable"

    [selectPopulate populationWithProbability 0.0]

let select (population: EvaluationTree list) : EvaluationTree list =
    let randomNumberGenerator = new Random()
    selectWithRandomNumberGenerator (fun () -> randomNumberGenerator.NextDouble()) population

let mutate = ignore

let combine = ignore

let generateIslands = ignore