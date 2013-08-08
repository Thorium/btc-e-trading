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

let apiv3MockInfo = @"{
    ""server_time"":1375428679,
    ""pairs"":{
        ""btc_usd"":{
            ""decimal_places"":3,""min_price"":0.1,""max_price"":400,""min_amount"":0.01,""hidden"":0,""fee"":0.2
        },
        ""btc_eur"":{
            ""decimal_places"":5,""min_price"":0.1,""max_price"":400,""min_amount"":0.1,""hidden"":0,""fee"":0.2
        },
        ""ltc_btc"":{
            ""decimal_places"":5,""min_price"":0.0001,""max_price"":10,""min_amount"":0.1,""hidden"":0,""fee"":0.2
        },
        ""ltc_usd"":{
            ""decimal_places"":6,""min_price"":0.0001,""max_price"":100,""min_amount"":0.1,""hidden"":0,""fee"":0.2
        },
        ""eur_usd"":{
            ""decimal_places"":5,""min_price"":1,""max_price"":2,""min_amount"":0.1,""hidden"":0,""fee"":0.2
        }
    }
}"

let apiv3MockPriceQuotes = @"{
	""btc_usd"":{
		""high"":96.8,""low"":93.999,""avg"":95.3995,""vol"":260830.03289,""vol_cur"":2732.85012,""last"":95.7,""buy"":95.7,""sell"":95.351,""updated"":1375708906
	},
	""btc_eur"":{
		""high"":76.1,""low"":74.02241,""avg"":75.061205,""vol"":1103.46817,""vol_cur"":14.68182,""last"":76.1,""buy"":75.71001,""sell"":75.30001,""updated"":1375708905
	},
	""ltc_btc"":{
		""high"":0.0289,""low"":0.02669,""avg"":0.027795,""vol"":4265.81843,""vol_cur"":153763.28179,""last"":0.02732,""buy"":0.02734,""sell"":0.02732,""updated"":1375708906
	},
	""ltc_usd"":{
		""high"":2.706,""low"":2.57011,""avg"":2.638055,""vol"":257067.84932,""vol_cur"":97972.0495,""last"":2.62,""buy"":2.62608,""sell"":2.610012,""updated"":1375708906
	},
	""eur_usd"":{
		""high"":1.278,""low"":1.2517,""avg"":1.26485,""vol"":3163.94629,""vol_cur"":2502.81582,""last"":1.26493,""buy"":1.2677,""sell"":1.25713,""updated"":1375708906
	}
}"

let mockDownloader (url: string) : string =
    let uri = new System.Uri(url)
    let apiMethod = uri.Segments.[3].ToLower().TrimEnd('/')
    match apiMethod with
        | "ticker" -> apiv3MockPriceQuotes
        | "info" -> apiv3MockInfo
        | _ -> failwith ("Unknown public api method: " + apiMethod)