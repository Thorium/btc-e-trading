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

    open TaLib.Library.PatternRecognition

    open BtceApiFramework.PublicBtceApi

    open BackTesting
    open GeneticProgramming

    let patternRecogniserFunctions = [|
        cdl3BlackCrows
        cdl3Inside
        cdl3LineStrike
        cdl3Outside
        cdl3StarsInSouth
        cdl3WhiteSoldiers
        cdlAbandonedBaby
        cdlAdvanceBlock
        cdlBeltHold
        cdlBreakaway
        cdlClosingMarubozu
        cdlConcealBabysWall
        cdlCounterAttack
        cdlDarkCloudCover
        cdlDoji
        cdlDojiStar
        cdlDragonflyDoji
        cdlEngulfing
        cdlEveningDojiStar
        cdlEveningStar
        cdlGapSideSideWhite
        cdlGravestoneDoji
        cdlHammer
        cdlHangingMan
        cdlHarami
        cdlHaramiCross
        cdlHignWave
        cdlHikkake
        cdlHikkakeMod
        cdlHomingPigeon
        cdlIdentical3Crows
        cdlInNeck
        cdlInvertedHammer
        cdlKicking
        cdlKickingByLength
        cdlLadderBottom
        cdlLongLeggedDoji
        cdlLongLine
        cdlMarubozu
        cdlMatHold
        cdlMatchingLow
        cdlMorningDojiStar
        cdlOnNeck
        cdlPiercing
        cdlRickshawMan
        cdlRiseFall3Methods
        cdlSeperatingLines
        cdlShootingStar
        cdlShortLine
        cdlSpinningTop
        cdlStalledPattern
        cdlStickSandwhich
        cdlTakuri
        cdlTasukiGap
        cdlThrusting
        cdlTristar
        cdlUnique3River
        cdlUpsideGap2Crows
        cdlXSideGap3Methods
    |]

    type PatternRecognitionFunction = int -> int -> float [] -> float [] -> float [] -> float [] -> TaLib.Library.Result<int [] * int * int>
    
    /// Array of Func objects containing the pattern recogniser functions so we can memoize the values the functions compute (function types cannot be compared for equality).
    let patternRecognisers = 
        patternRecogniserFunctions |> 
            Array.map (fun x -> new System.Func<PatternRecognitionFunction>(fun () -> x)) 
            
    let memoizePatternRecognitionComputation values =
        let cache = new System.Collections.Generic.Dictionary<System.Func<PatternRecognitionFunction>, _>()

        fun patternRecogniser ->
            let dog = Operators.id patternRecogniser
            let containsValue, containedValues = cache.TryGetValue(patternRecogniser)

            if containsValue then 
                containedValues
            else
                cache.[patternRecogniser] <- values
                values

    let operators = [|
        (>)
        (>=)
        (<)
        (<=)
    |]

    type Action =
        | Buy
        | Sell
        | DoNothing

    type OpenHighLowCloseOptional =
        {
            optionalHigh: Option<float>
            optionalLow: Option<float>
            optionalOpening: Option<float>
            optionalClosing: Option<float>
        }

    type OpenHighLowClose =
        {
            high: float []
            low: float []
            opening: float []
            closing: float []
        }

    type Func = OpenHighLowClose * int -> bool

    type FunctionArguments = {
        patternFunc: System.Func<PatternRecognitionFunction>
        operator: (int -> int -> bool)
        value: int
    }

    let func (arguments: FunctionArguments) (values, endIndex: int) =
        match (arguments.patternFunc.Invoke()) 0 endIndex values.high values.low values.opening values.closing with
            | TaLib.Library.Success(value, _, length) -> 
                if value.Length = 0 then
                    false
                else
                    arguments.operator value.[length - 1] arguments.value
            | TaLib.Library.Error(code) -> 
                failwith ("Error, failed to run pattern recogniser, returned code: " + code.ToString())

    type Function =
        | Leaf of Action
        | Branch of Func

    type Tree = EvaluationTree<Function, Action>

    let randomValueFromArray randomNumberGenerator (array: 'c []) =
        let index = randomNumberGenerator array.Length
        array.[index]

    let randomPatternRecogniser randomNumberGenerator = patternRecognisers |> randomValueFromArray randomNumberGenerator

    let randomOperator randomNumberGenerator = operators |> randomValueFromArray randomNumberGenerator
    
    let generateRandomFunc randomNumberGenerator =
        let comparisonValue = (randomNumberGenerator 101) - 100

        assert (comparisonValue >= -100 && comparisonValue <= 100)
    
        func {
            patternFunc = randomPatternRecogniser randomNumberGenerator
            operator = randomOperator randomNumberGenerator
            value = comparisonValue
        }

    let generateRandomAction randomNumberGenerator =
        let randomNumber = randomNumberGenerator 3
        assert (randomNumber >= 0 && randomNumber < 3)
        [Buy;Sell;DoNothing].[randomNumber]

    let growPatternRecogniserTree randomNumberGenerator =
        let branchGenerator = (fun _ -> generateRandomFunc randomNumberGenerator)
        let leafGenerator = (fun _ -> generateRandomAction randomNumberGenerator)
        populateByGrowth branchGenerator leafGenerator randomNumberGenerator

    type MutateValueOperator =
    | Plus
    | Minus

    /// <summary>
    /// Moves randomly either up or down by a random number between 0 and diff (inclusive) and kept given the range of minValue..maxValue (inclusive).
    /// </summary>
    let mutateValue randomNumberGenerator value diff maxValue minValue = 
        let operators = [|Plus;Minus|]

        let operator = operators.[randomNumberGenerator operators.Length]

        let (diff: int) = match operator with
                            | Plus when value + diff > maxValue -> maxValue - value
                            | Minus when value - diff < minValue -> minValue - value
                            | _ -> diff

        let diff = System.Math.Abs(diff) 

        let diff = randomNumberGenerator diff

        match operator with
        | Plus -> value + diff
        | Minus -> value - diff

    /// <summary>
    /// Generates a random function for branch nodes.
    /// </summary>
    let mutateFunctionArguments (functionArguments: FunctionArguments) randomNumberGenerator =
        let value = mutateValue randomNumberGenerator functionArguments.value 5 100 -100
        assert (value >= -100 && value <= 100)
        {
            patternFunc = randomPatternRecogniser randomNumberGenerator
            operator = randomOperator randomNumberGenerator
            value = value
        }

    /// <summary>
    /// Reads backtesting data and turns it into a list of open high low closing records.
    /// Because there is no closing of bitcoin exchanges we need our own, to do this we use the interval parameter.
    /// </summary>
    /// <param name="interval">How many backtesting records make up a "day". 
    /// e.g. if the value 15 will generate 1 high low closing data for each 15 btc records</param>
    let readIntervalData reader (readData: 'a -> seq<Record list>) interval =
        let toFloat value = System.Decimal.ToDouble value

        let updateValues values buy =
            let buy = Some(buy)
            {
                optionalHigh = if values.optionalHigh.IsNone || buy > values.optionalHigh then buy else values.optionalHigh
                optionalLow = if values.optionalLow.IsNone || buy < values.optionalLow then buy else values.optionalLow
                optionalOpening = if values.optionalOpening.IsNone then buy else values.optionalOpening
                optionalClosing = buy
            }

        let highs = new ResizeArray<float>()
        let lows = new ResizeArray<float>()
        let openings = new ResizeArray<float>()
        let closings = new ResizeArray<float>()

        let initialValue = { optionalHigh = None; optionalLow = None; optionalOpening = None; optionalClosing = None }, 1

        let getIntervalData (values, i) (x: Record list) =
            let (_, bitcoinQuote) = x.Head

            let buy = float(bitcoinQuote.buy)

            if i = interval then
                match updateValues values buy with
                    | { optionalHigh = Some(high); optionalLow = Some(low)
                        optionalOpening = Some(opening); optionalClosing = Some(closing) } ->
                            highs.Add(high)
                            lows.Add(low)
                            openings.Add(opening)
                            closings.Add(closing)
                    | _ -> ()

                initialValue
            else
                updateValues values buy, i + 1

        Seq.fold getIntervalData initialValue (readData reader) |> ignore

        {
            high = highs.ToArray() 
            low = lows.ToArray()
            opening = openings.ToArray()
            closing = closings.ToArray()
        }

    /// <summary>
    /// Calculates the value you would be left with after executing a series of trades represented by a list of actions.
    /// </summary>
    /// <param name="actions">List of actions ordered from left to right.</param>
    /// <param name="startValue">Value the evaluation will start from. 
    /// e.g. if you start on 100 and have a single action buy 10, then 90 will be returned.</param>
    let evaluateActions actions (startValue: decimal) =
        let evaluate runningTotal (action, price) =
            match action with
            | Buy -> runningTotal - price
            | Sell -> runningTotal + price
            | DoNothing -> runningTotal

        List.fold evaluate startValue actions
    
    /// <summary>
    /// Walks a tree depth first (evaluates a genetic program). 
    /// Each branch node is tested against a set of values; if the test passes the tree will walk the branch's children.
    /// The traversal stops and returns the value of the first leaf found, or DoNothing if no leaf is found. 
    /// </summary>
    /// <param name="values">The values that each branch node is to be tested against.</param>
    let evaluateTree tree values =
        let rec walkChildren func = function
            | child :: tail -> 
                let result = func child
                if result = DoNothing then
                    walkChildren func tail
                else
                    result
            | [] -> DoNothing

        let rec walkTree = function
            | TreeNode.Leaf(node) -> node.Data
            | TreeNode.Branch(node, children) -> 
                if node.Data values then
                    walkChildren walkTree children
                else
                    DoNothing

        walkTree tree.root

    let evaluateProgramAgainstIntervalData program values =
        assert(values.high.Length = values.low.Length 
            && values.low.Length = values.opening.Length 
            && values.opening.Length = values.closing.Length)

        let rec evaluateEachRecord i length actions =
            if i < length then
                let action = (evaluateTree program (values, i + 1)), decimal(values.closing.[i])

                evaluateEachRecord (i + 1) length (action :: actions)
            else
                actions

        evaluateEachRecord 0 values.high.Length []
    
    let fitness openHighLowClose program =
        let actions = evaluateProgramAgainstIntervalData program openHighLowClose

        let result = float(evaluateActions actions (decimal(0)))

        if result < 0.0 then
            decimal(-System.Math.Log(System.Math.Abs(result)))
        else if result > 0.0 then
            decimal(System.Math.Log(result))
        else
            decimal(0)