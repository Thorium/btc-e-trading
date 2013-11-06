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

module MockDownloader

let apiv3MockPriceQuotes: string =
    @"{
	    ""btc_usd"":{
		    ""high"":88.9,
		    ""low"":86.82,
		    ""avg"":87.86,
		    ""vol"":134881.26056,
		    ""vol_cur"":1536.15523,
		    ""last"":88,
		    ""buy"":88,
		    ""sell"":87.7,
		    ""updated"":1375005886
	    },
	    ""ltc_usd"":{
		    ""high"":2.692,
		    ""low"":2.6502,
		    ""avg"":2.6711,
		    ""vol"":54088.32538,
		    ""vol_cur"":20235.81144,
		    ""last"":2.67191,
		    ""buy"":2.671912,
		    ""sell"":2.67191,
		    ""updated"":1375005886
	    }
    }"

let apiv3MockInfo: string =
    @"{
        ""server_time"":1374928864,
        ""pairs"":{
            ""btc_usd"":{
                ""decimal_places"":3,""min_price"":0.1,""max_price"":400,""min_amount"":0.01,""hidden"":0,""fee"":0.2
            },
            ""btc_rur"":{
                ""decimal_places"":5,""min_price"":1,""max_price"":12000,""min_amount"":0.1,""hidden"":0,""fee"":0.2
            },
            ""btc_eur"":{
                ""decimal_places"":5,""min_price"":0.1,""max_price"":400,""min_amount"":0.1,""hidden"":0,""fee"":0.2
            }
        }
    }"

let apiv3MockDepth: string =
    @"{
	    ""btc_usd"":{
		    ""asks"":[
			    [88,52.05465611],
			    [88.14,0.4],
			    [88.147,0.02],
			    [88.148,1.00014957],
			    [88.155,1.03846942],
			    [88.161,0.034],
			    [88.184,0.229]
		    ],
            ""bids"":[
                [87.7,8.28483479],
                [87.693,2.67559182],
                [87.692,10],
                [87.69,4.92705873],
                [87.64,0.02],
                [87.562,0.01],
                [87.561,0.02]
            ]
        },
	    ""ltc_usd"":{
		    ""asks"":[
			    [2.671912,142.42960458],
			    [2.675,206.34564467],
			    [2.678206,0.1],
			    [2.679,16.16],
			    [2.68,61.5312542],
			    [2.6806,5],
			    [2.6809,382.54859506]
		    ],
            ""bids"":[
                [2.67191,174.25373677],
                [2.6615,101.47354207],
                [2.6614,0.2],
                [2.66066,4.22146707],
                [2.66065,0.1002],
                [2.660001,170.37205714],
                [2.66,239.23568006]
            ]
	    }
    }"

let apiv3MockRecentTrades: string = 
    @"{
		""btc_usd"":[
			{""type"":""ask"",""price"":87.7,""amount"":1.12083,""tid"":6455773,""timestamp"":1375005305},
			{""type"":""ask"",""price"":87.7,""amount"":1,""tid"":6455772,""timestamp"":1375005305},
			{""type"":""ask"",""price"":87.7,""amount"":0.879167,""tid"":6455771,""timestamp"":1375005305},
			{""type"":""ask"",""price"":87.7,""amount"":0.06265,""tid"":6455643,""timestamp"":1375004918},
			{""type"":""ask"",""price"":87.7,""amount"":0.0258954,""tid"":6455641,""timestamp"":1375004864},
			{""type"":""ask"",""price"":87.7,""amount"":0.02,""tid"":6455626,""timestamp"":1375004817},
			{""type"":""bid"",""price"":88,""amount"":3.39478,""tid"":6455540,""timestamp"":1375003848},
			{""type"":""ask"",""price"":87.7,""amount"":0.0122875,""tid"":6455536,""timestamp"":1375003744},
			{""type"":""ask"",""price"":88.1,""amount"":5.93464,""tid"":6455496,""timestamp"":1375003436},
			{""type"":""bid"",""price"":88.1,""amount"":0.1,""tid"":6455495,""timestamp"":1375003435},
			{""type"":""bid"",""price"":88.1,""amount"":1,""tid"":6455494,""timestamp"":1375003435},
			{""type"":""bid"",""price"":88.1,""amount"":4.96536,""tid"":6455493,""timestamp"":1375003435},
			{""type"":""bid"",""price"":88.1,""amount"":3.6837,""tid"":6455490,""timestamp"":1375003237}
		],
		""ltc_usd"":[
			{""type"":""ask"",""price"":2.6609,""amount"":5.34364,""tid"":6455770,""timestamp"":1375005299},
			{""type"":""bid"",""price"":2.67191,""amount"":0.512,""tid"":6455646,""timestamp"":1375004956},
			{""type"":""bid"",""price"":2.67191,""amount"":0.848262,""tid"":6455644,""timestamp"":1375004933},
			{""type"":""ask"",""price"":2.6601,""amount"":15,""tid"":6455435,""timestamp"":1375002358},
			{""type"":""ask"",""price"":2.6602,""amount"":0.5,""tid"":6455432,""timestamp"":1375002173},
			{""type"":""ask"",""price"":2.6602,""amount"":9.5,""tid"":6455431,""timestamp"":1375002173},
			{""type"":""ask"",""price"":2.6601,""amount"":50,""tid"":6455426,""timestamp"":1375002036},
			{""type"":""ask"",""price"":2.66,""amount"":2.7738,""tid"":6455407,""timestamp"":1375001596},
			{""type"":""ask"",""price"":2.66,""amount"":12,""tid"":6455401,""timestamp"":1375001508},
			{""type"":""ask"",""price"":2.66,""amount"":1.00011,""tid"":6455246,""timestamp"":1375000228},
			{""type"":""ask"",""price"":2.66,""amount"":160,""tid"":6455224,""timestamp"":1374999651},
			{""type"":""ask"",""price"":2.66,""amount"":21.8531,""tid"":6455211,""timestamp"":1374999454},
			{""type"":""ask"",""price"":2.66,""amount"":4,""tid"":6455174,""timestamp"":1374998929}
		]
	}"

let mockDownloader (url: string) : string =
    let uri = new System.Uri(url)
    let apiMethod = uri.Segments.[3].ToLower().TrimEnd('/')
    match apiMethod with
        | "ticker" -> apiv3MockPriceQuotes
        | "info" -> apiv3MockInfo
        | "depth" -> apiv3MockDepth
        | "trades" -> apiv3MockRecentTrades
        | _ -> failwith ("Unknown public api method: " + apiMethod)