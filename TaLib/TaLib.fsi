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

    /// Default int value to use for optional int parameters
    val public taIntegerDefault: int

    /// Default float value to use for optional float parameters
    val public taRealDefault: float

    type Result<'T> =
        | Success of 'T
        | Error of Core.RetCode

    module MomentumIndicator =
        val adx: float [] -> float [] -> float [] -> int -> Result<float []>   
        val adxr: float [] -> float [] -> float [] -> int -> Result<float []>        

    module CycleIndicator =
        val htDcPeriod: float [] -> Result<float []>
        val htDcPhase: float [] -> Result<float []>
        val htPhasor: float [] -> Result<float [] * float []>
        val htSine: float [] -> Result<float [] * float []>
        val htTrendMode: float [] -> Result<int []>

    module PatternRecognition =
        val cdl2Crows: float [] -> float [] -> float [] -> float [] -> Result<int []>    
        val cdl3BlackCrows: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdl3Inside: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdl3LineStrike: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdl3Outside: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdl3StarsInSouth: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdl3WhiteSoldiers: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlAbandonedBabyWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []>  
        val cdlAbandonedBaby: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlAdvanceBlock: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlBeltHold: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlBreakaway: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlClosingMarubozu: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlConcealBabysWall: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlCounterAttack: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlDarkCloudCoverWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []>  
        val cdlDarkCloudCover: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlDoji: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlDojiStar: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlDragonflyDoji: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlEngulfing: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlEveningDojiStarWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []>  
        val cdlEveningDojiStar: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlEveningStarWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []>  
        val cdlEveningStar: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlGapSideSideWhite: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlGravestoneDoji: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHammer: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHangingMan: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHarami: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHaramiCross: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHignWave: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHikkake: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHikkakeMod: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlHomingPigeon: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlIdentical3Crows: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlInNeck: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlInvertedHammer: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlKicking: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlKickingByLength: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlLadderBottom: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlLongLeggedDoji: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlLongLine: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlMarubozu: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlMatchingLow: float [] -> float [] -> float [] -> float [] -> Result<int []>  
        val cdlMatHoldWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []> 
        val cdlMatHold: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlMorningDojiStarWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []> 
        val cdlMorningDojiStar: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlMorningStarWithPenetration: float [] -> float [] -> float [] -> float [] -> float -> Result<int []> 
        val cdlMorningStar: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlOnNeck: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlPiercing: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlRickshawMan: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlRiseFall3Methods: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlSeperatingLines: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlShootingStar: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlShortLine: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlSpinningTop: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlStalledPattern: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlStickSandwhich: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlTakuri: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlTasukiGap: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlThrusting: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlTristar: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlUnique3River: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlUpsideGap2Crows: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        val cdlXSideGap3Methods: float [] -> float [] -> float [] -> float [] -> Result<int []> 
        
    module PriceTransformation =
        val avgPrice: float [] -> float [] -> float [] -> float [] -> Result<float []>     
        val medPrice: float [] -> float [] -> Result<float []>  
        val typPrice: float [] -> float [] -> float [] -> Result<float []>  
        val wclPrice: float [] -> float [] -> float [] -> Result<float []>     

    module Statistics =
        val beta: float [] -> float [] -> int -> Result<float []>
        val correl: float [] -> float [] -> int -> Result<float []>
        val linearReg: float [] -> int -> Result<float []>
        val linearRegAngle: float [] -> int -> Result<float []>
        val linearRegIntercept: float [] -> int -> Result<float []>
        val linearRegSlope: float [] -> int -> Result<float []>
        val stdDev: float [] -> int -> float -> Result<float []>
        val tsf: float [] -> int -> Result<float []>
        val variance: float [] -> int -> float -> Result<float []>

    module VolumeIndicators =
        val ad: float [] -> float [] -> float [] -> float [] -> Result<float []>
        val adOsc: float [] -> float [] -> float [] -> float[] -> int -> int -> Result<float []>
        val obv: float [] -> float [] -> Result<float []>
        
    module VolatilityIndicator =
        val atr: float [] -> float [] -> float [] -> int -> Result<float []>
        val natr: float [] -> float [] -> float [] -> int -> Result<float []>
        val trueRange: float [] -> float [] -> float [] -> Result<float []>

    module Maths =
        val max: float [] -> int -> Result<float []>
        val min: float [] -> int -> Result<float []>
        val sum: float [] -> int -> Result<float []>
        val maxIndex: float [] -> int -> Result<int []>
        val minIndex: float [] -> int -> Result<int []>
        val minMax: float [] -> int -> Result<float [] * float []>
        val minMaxIndex: float [] -> int -> Result<int [] * int []>
        val add: float [] -> float [] -> Result<float []>
        val sub: float [] -> float [] -> Result<float []>
        val mult: float [] -> float [] -> Result<float []>
        val div: float [] -> float [] -> Result<float []>