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

namespace TaLib

module Library = 
    open TicTacTec.TA.Library
    open System

    let taIntegerDefault = Int32.MinValue
    let taRealDefault = -4e+37

    type Result<'T> =
        | Success of 'T
        | Error of Core.RetCode

    let apply func size = 
        let startIndex = ref 0
        let numberOfElements = ref 0
        let mutable resultData: 'T [] = Array.create<'T> (size) Unchecked.defaultof<'T>

        let result = func startIndex numberOfElements resultData

        if result = Core.RetCode.Success then
            Success(resultData.[!startIndex..(!startIndex) + (!numberOfElements) - 1])
        else
            Error(result)

    let apply2 func length = 
        let startIndex = ref 0
        let endIndex = ref 0
        let mutable resultData: 'T [] = Array.create<'T> (length) Unchecked.defaultof<'T>
        let mutable resultData2: 'T [] = Array.create<'T> (length) Unchecked.defaultof<'T>

        let result = func startIndex endIndex resultData resultData2

        if result = Core.RetCode.Success then
            Success(resultData.[!startIndex..(!endIndex) - 1], resultData2.[!startIndex..(!endIndex) - 1])
        else
            Error(result)

    let apply3 func length = 
        let startIndex = ref 0
        let endIndex = ref 0
        let mutable resultData: 'T [] = Array.create<'T> (length) Unchecked.defaultof<'T>
        let mutable resultData2: 'T [] = Array.create<'T> (length) Unchecked.defaultof<'T>
        let mutable resultData3: 'T [] = Array.create<'T> (length) Unchecked.defaultof<'T>

        let result = func startIndex endIndex resultData resultData2 resultData3

        if result = Core.RetCode.Success then
            Success(resultData.[!startIndex..(!endIndex) - 1], resultData2.[!startIndex..(!endIndex) - 1], resultData3.[!startIndex..(!endIndex) - 1])
        else
            Error(result)

            (*
    module Overlap =

        let movingAverageVariablePeriod (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.MovingAverageVariablePeriod(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let movingAverage (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.MovingAverage(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let bbands (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let HtTrendline (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let MidPoint (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let MidPrice (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let Sar (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let SarExt (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Bbands(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length
            *)
    module MomentumIndicator =

        let adx (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Adx(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let adxr (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Adxr(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length
            
        let apo (data: float []) (optionalFastPeriod: int) (optionalSlowPeriod: int) (movingAverage: Core.MAType) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Apo(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) data optionalFastPeriod optionalSlowPeriod movingAverage) length
            
        let aroon (high: float []) (low: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Aroon(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply2 (operation 0 (length - 1) high low optionalTimePeriod) length

        let aroonOsc (high: float []) (low: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.AroonOsc(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low optionalTimePeriod) length

        let bop (opening: float []) (high: float []) (low: float []) (closing: float []) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Bop(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) opening high low closing) length

        let cci (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Cci(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let cmo (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Cmo(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let dx (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Dx(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let macd (data: float []) (optionalFastPeriod: int) (optionalSlowPeriod: int) (optionalSignalPeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 -> Core.Macd(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
            apply3 (operation 0 (length - 1) data optionalFastPeriod optionalSlowPeriod optionalSignalPeriod) length

        let macdExt (data: float []) (optionalFastPeriod: int) (optionalFastMovingAverage: Core.MAType) (optionalSlowPeriod: int) (optionalSlowMovingAverage: Core.MAType) (optionalSignalPeriod: int) (optionalSignalMovingAverage: Core.MAType) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> Core.MacdExt(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14))
            apply3 (operation 0 (length - 1) data optionalFastPeriod optionalFastMovingAverage optionalSlowPeriod optionalSlowMovingAverage optionalSignalPeriod optionalSignalMovingAverage) length

        let macdFix (data: float []) (optionalSignalPeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.MacdFix(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply3 (operation 0 (length - 1) data optionalSignalPeriod) length

        let mfi (high: float []) (low: float []) (closing: float []) (volume: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 -> Core.Mfi(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
            apply (operation 0 (length - 1) high low closing volume optionalTimePeriod) length

        let minusDI (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.MinusDI(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let minusDM (high: float []) (low: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.MinusDM(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low optionalTimePeriod) length

        let mom (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Mom(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let plusDI (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.PlusDI(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let plusDM (high: float []) (low: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.PlusDM(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low optionalTimePeriod) length

        let ppo (data: float []) (optionalFastPeriod: int) (optionalSlowPeriod: int) (movingAverage: Core.MAType) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Ppo(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) data optionalFastPeriod optionalSlowPeriod movingAverage) length

        let roc (data: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Roc(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let rocP (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.RocP(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let rocR (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.RocR(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let rocR100 (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.RocR100(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let rsi (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Rsi(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length
            (*
        let stoch (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> Core.Stoch(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14))
            apply2 (operation 0 (length - 1) high low closing optionalTimePeriod) length
            *)
        let StochF (data: float []) (optionalFastPeriod: int) (optionalSlowPeriod: int) (movingAverage: Core.MAType) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Apo(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) data optionalFastPeriod optionalSlowPeriod movingAverage) length
            (*
        let stochRsi (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 -> Core.StochRsi(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
            apply2 (operation 0 (length - 1) high low closing optionalTimePeriod) length
            *)
        let trix (data: float []) (optionalTimePeriod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Trix(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimePeriod) length

        let ultOsc (high: float []) (low: float []) (closing: float []) (optionalTimePeriod1: int) (optionalTimePeriod2: int) (optionalTimePeriod3: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 -> Core.UltOsc(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod1 optionalTimePeriod2 optionalTimePeriod3) length

        let willR (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.WillR(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length
            
    module CycleIndicator =
        
        let htDcPeriod (data: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 -> Core.HtDcPeriod(x1,x2,x3,x4,x5,x6))
            apply (operation 0 (length - 1) data) length

        let htDcPhase (data: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 -> Core.HtDcPhase(x1,x2,x3,x4,x5,x6))
            apply (operation 0 (length - 1) data) length

        let htPhasor (data: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.HtPhasor(x1,x2,x3,x4,x5,x6,x7))
            apply2 (operation 0 (length - 1) data) length

        let htSine (data: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.HtSine(x1,x2,x3,x4,x5,x6,x7))
            apply2 (operation 0 (length - 1) data) length

        let htTrendMode (data: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 -> Core.HtTrendMode(x1,x2,x3,x4,x5,x6))
            apply (operation 0 (length - 1) data) length
            
    module PatternRecognition =

        let operate high low opening (closing: float []) operation =
            let length = closing.Length
            apply (operation 0 (length - 1) opening high low closing) length

        let operateWithPenetration high low opening (closing: float []) penetration operation =
            let length = closing.Length
            apply (operation 0 (length - 1) opening high low closing penetration) length

        let cdl2Crows (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl2Crows(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdl3BlackCrows (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3BlackCrows(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdl3Inside (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3Inside(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdl3LineStrike (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3LineStrike(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdl3Outside (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3Outside(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdl3StarsInSouth (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3StarsInSouth(x1,x2,x3,x4,x5,x6,x7,x8,x9))
           
        let cdl3WhiteSoldiers (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.Cdl3WhiteSoldiers(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlAbandonedBabyWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlAbandonedBaby(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlAbandonedBaby high low opening closing = cdlAbandonedBabyWithPenetration high low opening closing taRealDefault

        let cdlAdvanceBlock (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlAdvanceBlock(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlBeltHold (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlBeltHold(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlBreakaway (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlBreakaway(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlClosingMarubozu (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlClosingMarubozu(x1,x2,x3,x4,x5,x6,x7,x8,x9))
             
        let cdlConcealBabysWall (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlConcealBabysWall(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlCounterAttack (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlCounterAttack(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlDarkCloudCoverWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlDarkCloudCover(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlDarkCloudCover high low opening closing = cdlDarkCloudCoverWithPenetration high low opening closing taRealDefault

        let cdlDoji (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlDoji(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlDojiStar (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlDojiStar(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            
        let cdlDragonflyDoji (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlDragonflyDoji(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlEngulfing (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlEngulfing(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlEveningDojiStarWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlEveningDojiStar(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlEveningDojiStar high low opening closing = cdlEveningDojiStarWithPenetration high low opening closing taRealDefault

        let cdlEveningStarWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlEveningStar(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlEveningStar high low opening closing = cdlEveningStarWithPenetration high low opening closing taRealDefault

        let cdlGapSideSideWhite (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlGapSideSideWhite(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlGravestoneDoji (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlGravestoneDoji(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHammer (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHammer(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHangingMan (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHangingMan(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHarami (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHarami(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHaramiCross (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHaramiCross(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHignWave (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHignWave(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHikkake (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHikkake(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHikkakeMod (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHikkakeMod(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlHomingPigeon (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlHomingPigeon(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlIdentical3Crows (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlIdentical3Crows(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlInNeck (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlInNeck(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlInvertedHammer (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlInvertedHammer(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlKicking (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlKicking(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlKickingByLength (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlKickingByLength(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlLadderBottom (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlLadderBottom(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlLongLeggedDoji (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlLongLeggedDoji(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlLongLine (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlLongLine(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlMarubozu (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlMarubozu(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlMatchingLow (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlMatchingLow(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlMatHoldWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlMatHold(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlMatHold high low opening closing = cdlMatHoldWithPenetration high low opening closing taRealDefault

        let cdlMorningDojiStarWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlMorningDojiStar(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlMorningDojiStar high low opening closing = cdlMorningDojiStarWithPenetration high low opening closing taRealDefault

        let cdlMorningStarWithPenetration (high: float []) (low: float []) (opening: float []) (closing: float []) penetration =
            operateWithPenetration high low opening closing penetration (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> Core.CdlMorningStar(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
    
        let cdlMorningStar high low opening closing = cdlMorningStarWithPenetration high low opening closing taRealDefault

        let cdlOnNeck (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlOnNeck(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlPiercing (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlPiercing(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlRickshawMan (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlRickshawMan(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlRiseFall3Methods (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlRiseFall3Methods(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlSeperatingLines (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlSeperatingLines(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlShootingStar (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlShootingStar(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlShortLine (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlShortLine(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlSpinningTop (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlSpinningTop(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlStalledPattern (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlStalledPattern(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlStickSandwhich (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlStickSandwhich(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlTakuri (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlTakuri(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlTasukiGap (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlTasukiGap(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlThrusting (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlThrusting(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlTristar (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlTristar(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlUnique3River (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlUnique3River(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlUpsideGap2Crows (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlUpsideGap2Crows(x1,x2,x3,x4,x5,x6,x7,x8,x9))

        let cdlXSideGap3Methods (high: float []) (low: float []) (opening: float []) (closing: float []) =
            operate high low opening closing (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> Core.CdlXSideGap3Methods(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            
    module PriceTransformation =
        
        let avgPrice (high: float []) (low: float []) (opening: float []) (closing: float []) =
            let length = closing.Length
            let operation = (fun x1 x2 x3 (x4: float []) x5 x6 x7 x8 x9 -> Core.AvgPrice(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) opening high low closing) length

        let medPrice (high: float []) (low: float []) =
            let length = high.Length
            let operation = (fun x1 x2 x3 (x4: float []) x5 x6 x7 -> Core.MedPrice(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) high low) length
            
        let typPrice (high: float []) (low: float []) (closing: float []) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.TypPrice(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low closing) length
        
        let wclPrice (high: float []) (low: float []) (closing: float []) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.WclPrice(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low closing) length

    module Statistics =

        let beta (lhs: float []) (rhs: float []) (optionalTimeperiod: int) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.Beta(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) lhs rhs optionalTimeperiod) length

        let correl (lhs: float []) (rhs: float []) (optionalTimeperiod: int) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.Correl(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) lhs rhs optionalTimeperiod) length

        let linearReg (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.LinearReg(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let linearRegAngle (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.LinearRegAngle(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let linearRegIntercept (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.LinearRegIntercept(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let linearRegSlope (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.LinearRegSlope(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let stdDev (data: float []) (optionalTimeperiod: int) (numberOfDeviations: float) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.StdDev(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) data optionalTimeperiod numberOfDeviations) length

        let tsf (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Tsf(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let variance (data: float []) (optionalTimeperiod: int) (numberOfDeviations: float) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.Variance(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) data optionalTimeperiod numberOfDeviations) length
    
    module VolumeIndicators =
        
        let ad (high: float []) (low: float []) (closing: float []) (volume: float []) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Ad(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing volume) length

        let adOsc (high: float []) (low: float []) (closing: float []) (volume: float []) optionalFastPeriod optionalSlowPeriod =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 x10 x11 -> Core.AdOsc(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
            apply (operation 0 (length - 1) high low closing volume optionalFastPeriod optionalSlowPeriod) length

        let obv (data: float []) (volume: float []) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Obv(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data volume) length

    module VolatilityIndicator =
        
        let atr (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Atr(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let natr (high: float []) (low: float []) (closing: float []) (optionalTimePeriod: int) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 x9 -> Core.Natr(x1,x2,x3,x4,x5,x6,x7,x8,x9))
            apply (operation 0 (length - 1) high low closing optionalTimePeriod) length

        let trueRange (high: float []) (low: float []) (closing: float []) =
            let length = high.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.TrueRange(x1,x2,x3,x4,x5,x6,x7,x8))
            apply (operation 0 (length - 1) high low closing) length

    module Maths =
        
        let max (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Max(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let min (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Min(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let sum (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Sum(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let maxIndex (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.MaxIndex(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let minIndex (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.MinIndex(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) data optionalTimeperiod) length

        let minMax (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.MinMax(x1,x2,x3,x4,x5,x6,x7,x8))
            apply2 (operation 0 (length - 1) data optionalTimeperiod) length

        let minMaxIndex (data: float []) (optionalTimeperiod: int) =
            let length = data.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 x8 -> Core.MinMaxIndex(x1,x2,x3,x4,x5,x6,x7,x8))
            apply2 (operation 0 (length - 1) data optionalTimeperiod) length

        let add (lhs: float []) (rhs: float []) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Add(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) lhs rhs) length

        let sub (lhs: float []) (rhs: float []) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Sub(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) lhs rhs) length

        let mult (lhs: float []) (rhs: float []) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Mult(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) lhs rhs) length

        let div (lhs: float []) (rhs: float []) =
            let length = lhs.Length
            let operation = (fun x1 x2 (x3: float []) x4 x5 x6 x7 -> Core.Div(x1,x2,x3,x4,x5,x6,x7))
            apply (operation 0 (length - 1) lhs rhs) length