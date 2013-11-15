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

namespace TradingFramework

module PatternRecognitionGP =

    open GeneticProgramming
    open BackTesting

    open BtceApiFramework.PublicBtceApi

    type public OpenHighLowCloseOptional =
        {
            optionalHigh: Option<float>
            optionalLow: Option<float>
            optionalOpening: Option<float>
            optionalClosing: Option<float>
        }

    type public OpenHighLowClose =
        {
            high: float []
            low: float []
            opening: float []
            closing: float []
        }

    type public Func = OpenHighLowClose * int -> bool

    type public PatternRecognitionFunction = float [] -> float [] -> float [] -> float [] -> TaLib.Library.Result<int []>

    type public FunctionArguments = 
        {
            patternFunc: PatternRecognitionFunction
            operator: (int -> int -> bool)
            value: int
        }

    type public Action =
        | Buy
        | Sell
        | DoNothing

    /// <summary>
    /// Moves randomly either up or down by a random number between 0 and diff (inclusive) and kept given the range of minValue..maxValue (inclusive).
    /// </summary>
    val public mutateValue: randomNumberGenerator:(int -> int) -> value:int -> diff:int -> maxValue:int -> minValue:int -> int

    val public mutateFunctionArguments: functionArguments:FunctionArguments -> randomNumberGenerator:(int -> int) -> FunctionArguments

    /// <summary>
    /// Calculates the value you would be left with after executing a series of trades represented by a list of actions.
    /// </summary>
    /// <param name="actions">List of actions ordered from left to right.</param>
    /// <param name="startValue">Value the evaluation will start from. 
    /// e.g. if you start on 100 and have a single action buy 10, then 90 will be returned.</param>
    val public evaluateActions: actions:(Action * decimal) list -> startValue:decimal -> decimal

    /// <summary>
    /// Walks a tree depth first (evaluates a genetic program). 
    /// Each branch node is tested against a set of values; if the test passes the tree will walk the branch's children.
    /// The traversal stops and returns the value of the first leaf found, or DoNothing if no leaf is found. 
    /// </summary>
    /// <param name="values">The values that each branch node is to be tested against.</param>
    val public evaluateTree: tree:EvaluationTree<('a -> bool), Action> -> values:'a -> Action

    /// <summary>
    /// Reads backtesting data and turns it into a list of open high low closing records.
    /// Because there is no closing of bitcoin exchanges we need our own, to do this we use the interval parameter.
    /// </summary>
    /// <param name="interval">How many backtesting records make up a "day". 
    /// e.g. if the value 15 will generate 1 high low closing data for each 15 btc records</param>
    val public readIntervalData: reader:'a -> readData:('a -> seq<Record list>) -> interval:int -> OpenHighLowClose

    val public growPatternRecogniserTree: randomNumberGenerator:(int -> int) -> (int -> int -> int -> EvaluationTree<Func, Action>)

    val public fitness: openHighLowClose:OpenHighLowClose -> EvaluationTree<Func, Action> -> decimal