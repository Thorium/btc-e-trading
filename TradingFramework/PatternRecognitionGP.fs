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

    let patternRecognisers = [|
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

    type PatternRecognitionFunction = float [] -> float [] -> float [] -> float [] -> TaLib.Library.Result<int []>

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

    type Func = OpenHighLowClose -> bool

    type FunctionArguments = {
        patternFunc: PatternRecognitionFunction
        operator: (int -> int -> bool)
        value: int
    }

    let func (arguments: FunctionArguments) values =
        match arguments.patternFunc values.high values.low values.opening values.closing with
            | TaLib.Library.Success(value) -> 
                arguments.operator value.[value.Length] arguments.value
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

    let mutateValue randomNumberGenerator value = 
        let randomNumber = (randomNumberGenerator 32) + 1
        assert (randomNumber > 0 && randomNumber <= 32)
        value ^^^ int(1.0 ** float(randomNumber))

    /// <summary>
    /// Generates a random function for branch nodes.
    /// </summary>
    let mutateFunctionArguments (functionArguments: FunctionArguments) randomNumberGenerator =
        let value = mutateValue randomNumberGenerator functionArguments.value
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
    let readIntervalData reader readData interval =
        let toFloat value = System.Decimal.ToDouble value

        let updateValues values buy =
            let buy = Some(buy)
            {
                optionalHigh = if buy > values.optionalHigh then buy else values.optionalHigh
                optionalLow = if buy < values.optionalLow then buy else values.optionalLow
                optionalOpening = if values.optionalOpening.IsNone then buy else values.optionalOpening
                optionalClosing = buy
            }

        let highs = new ResizeArray<float>()
        let lows = new ResizeArray<float>()
        let openings = new ResizeArray<float>()
        let closings = new ResizeArray<float>()

        readData reader (fun (x: Record list) i values -> 
            let (_, bitcoinQuote) = x.Head

            let buy = toFloat bitcoinQuote.buy

            if i = interval then
                match values with
                    | { optionalHigh = Some(high); optionalLow = Some(low)
                        optionalOpening = Some(opening); optionalClosing = Some(closing) } ->
                            highs.Add(high)
                            lows.Add(low)
                            openings.Add(opening)
                            closings.Add(closing)
                    | _ -> ()

                { optionalHigh = None; optionalLow = None; optionalOpening = None; optionalClosing = None }, 1
            else
                updateValues values buy, i + 1)

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
        let evaluate = fun runningTotal (action, price) ->
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
        assert(values.high.Length = values.low.Length && values.low.Length = values.opening.Length 
            && values.opening.Length = values.closing.Length)

        let rec evaluateEachRecord i actions =
            if i >= 0 then
                let action = evaluateTree program (values.high.[0..i], values.low.[0..i], values.opening.[0..i], values.closing.[0..i]), decimal(values.closing.[i])
                evaluateEachRecord (i - 1) (action :: actions)
            else
                actions

        evaluateEachRecord (values.high.Length - 1) []
    
    let fitness program readData (filename: string) =
        use sr = new System.IO.StreamReader(filename)
        let reader () =
            if not sr.EndOfStream then
                Some(sr.ReadLine())
            else
                None

        let interval = 15

        let values = readIntervalData reader readData interval

        let actions = evaluateProgramAgainstIntervalData program values

        let result = evaluateActions actions (new System.Decimal(0))

        1