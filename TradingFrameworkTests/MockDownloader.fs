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
            ""decimal_places"":3,""min_price"":0.1,""max_price"":400,""min_amount"":0.01,""hidden"":0,""fee"":0.1
        },
        ""btc_eur"":{
            ""decimal_places"":5,""min_price"":0.1,""max_price"":400,""min_amount"":0.1,""hidden"":0,""fee"":0.1
        },
        ""ltc_btc"":{
            ""decimal_places"":5,""min_price"":0.0001,""max_price"":10,""min_amount"":0.1,""hidden"":0,""fee"":0.1
        },
        ""ltc_usd"":{
            ""decimal_places"":6,""min_price"":0.0001,""max_price"":100,""min_amount"":0.1,""hidden"":0,""fee"":0.1
        },
        ""eur_usd"":{
            ""decimal_places"":5,""min_price"":1,""max_price"":2,""min_amount"":0.1,""hidden"":0,""fee"":0.1
        }
    }
}"

let apiv3MockPriceQuotes = @"{
	""btc_usd"":
	{
		""high"":96.969,""low"":93.211,""avg"":95.09,""vol"":410734.80816,""vol_cur"":4321.84986,""last"":95.45,""buy"":100,""sell"":90,""updated"":1375430811
	},
	""btc_eur"":
	{
		""high"":77.39499,""low"":73.9999,""avg"":75.697445,""vol"":16917.10877,""vol_cur"":222.59421,""last"":75,""buy"":100,""sell"":100,""updated"":1375430810
	},
	""ltc_btc"":
	{
		""high"":0.0288,""low"":0.0274,""avg"":0.0281,""vol"":2814.97356,""vol_cur"":100652.45131,""last"":0.02792,""buy"":10,""sell"":10,""updated"":1375430811
	},
	""ltc_usd"":
	{
		""high"":2.69,""low"":2.626,""avg"":2.658,""vol"":88361.82891,""vol_cur"":33231.11484,""last"":2.65,""buy"":10,""sell"":10,""updated"":1375430811
	},
	""eur_usd"":
	{
		""high"":1.2588,""low"":1.2469,""avg"":1.25285,""vol"":8780.35387,""vol_cur"":7021.91538,""last"":1.2571,""buy"":10,""sell"":10,""updated"":1375430811
	}
}"

let mockDownloader (url: string) : string =
    let uri = new System.Uri(url)
    let apiMethod = uri.Segments.[3].ToLower().TrimEnd('/')
    match apiMethod with
        | "ticker" -> apiv3MockPriceQuotes
        | "info" -> apiv3MockInfo
        | _ -> failwith ("Unknown public api method: " + apiMethod)