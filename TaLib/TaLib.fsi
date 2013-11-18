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
        val adx: float [] -> float [] -> float [] -> int -> Result<float [] * int * int>   
        val adxr: float [] -> float [] -> float [] -> int -> Result<float [] * int * int>        

    module CycleIndicator =
        val htDcPeriod: float [] -> Result<float [] * int * int>
        val htDcPhase: float [] -> Result<float [] * int * int>
        val htPhasor: float [] -> Result<float [] * float [] * int * int>
        val htSine: float [] -> Result<float [] * float [] * int * int>
        val htTrendMode: float [] -> Result<int [] * int * int>

    module PatternRecognition =
        val cdl2Crows: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>    
        val cdl3BlackCrows: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdl3Inside: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdl3LineStrike: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdl3Outside: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdl3StarsInSouth: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdl3WhiteSoldiers: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlAbandonedBabyWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int>  
        val cdlAbandonedBaby: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlAdvanceBlock: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlBeltHold: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlBreakaway: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlClosingMarubozu: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlConcealBabysWall: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlCounterAttack: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlDarkCloudCoverWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int>  
        val cdlDarkCloudCover: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlDoji: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlDojiStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlDragonflyDoji: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlEngulfing: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlEveningDojiStarWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int>  
        val cdlEveningDojiStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlEveningStarWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int>  
        val cdlEveningStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlGapSideSideWhite: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlGravestoneDoji: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHammer: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHangingMan: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHarami: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHaramiCross: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHignWave: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHikkake: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHikkakeMod: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlHomingPigeon: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlIdentical3Crows: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlInNeck: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlInvertedHammer: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlKicking: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlKickingByLength: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlLadderBottom: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlLongLeggedDoji: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlLongLine: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlMarubozu: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlMatchingLow: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int>  
        val cdlMatHoldWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int> 
        val cdlMatHold: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlMorningDojiStarWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int> 
        val cdlMorningDojiStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlMorningStarWithPenetration: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> float -> Result<int [] * int * int> 
        val cdlMorningStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlOnNeck: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlPiercing: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlRickshawMan: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlRiseFall3Methods: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlSeperatingLines: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlShootingStar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlShortLine: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlSpinningTop: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlStalledPattern: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlStickSandwhich: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlTakuri: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlTasukiGap: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlThrusting: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlTristar: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlUnique3River: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlUpsideGap2Crows: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        val cdlXSideGap3Methods: start:int -> length:int -> high:float [] -> low: float [] -> opening:float [] -> closing:float [] -> Result<int [] * int * int> 
        
    module PriceTransformation =
        val avgPrice: float [] -> float [] -> float [] -> float [] -> Result<float [] * int * int>     
        val medPrice: float [] -> float [] -> Result<float [] * int * int>  
        val typPrice: float [] -> float [] -> float [] -> Result<float [] * int * int>  
        val wclPrice: float [] -> float [] -> float [] -> Result<float [] * int * int>     

    module Statistics =
        val beta: float [] -> float [] -> int -> Result<float [] * int * int>
        val correl: float [] -> float [] -> int -> Result<float [] * int * int>
        val linearReg: float [] -> int -> Result<float [] * int * int>
        val linearRegAngle: float [] -> int -> Result<float [] * int * int>
        val linearRegIntercept: float [] -> int -> Result<float [] * int * int>
        val linearRegSlope: float [] -> int -> Result<float [] * int * int>
        val stdDev: float [] -> int -> float -> Result<float [] * int * int>
        val tsf: float [] -> int -> Result<float [] * int * int>
        val variance: float [] -> int -> float -> Result<float [] * int * int>

    module VolumeIndicators =
        val ad: float [] -> float [] -> float [] -> float [] -> Result<float [] * int * int>
        val adOsc: float [] -> float [] -> float [] -> float[] -> int -> int -> Result<float [] * int * int>
        val obv: float [] -> float [] -> Result<float [] * int * int>
        
    module VolatilityIndicator =
        val atr: float [] -> float [] -> float [] -> int -> Result<float [] * int * int>
        val natr: float [] -> float [] -> float [] -> int -> Result<float [] * int * int>
        val trueRange: float [] -> float [] -> float [] -> Result<float [] * int * int>

    module Maths =
        val max: float [] -> int -> Result<float [] * int * int>
        val min: float [] -> int -> Result<float [] * int * int>
        val sum: float [] -> int -> Result<float [] * int * int>
        val maxIndex: float [] -> int -> Result<int [] * int * int>
        val minIndex: float [] -> int -> Result<int [] * int * int>
        val minMax: float [] -> int -> Result<float [] * float [] * int * int>
        val minMaxIndex: float [] -> int -> Result<int [] * int [] * int * int>
        val add: float [] -> float [] -> Result<float [] * int * int>
        val sub: float [] -> float [] -> Result<float [] * int * int>
        val mult: float [] -> float [] -> Result<float [] * int * int>
        val div: float [] -> float [] -> Result<float [] * int * int>