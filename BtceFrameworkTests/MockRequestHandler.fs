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

module MockRequestHandler

open System

let getInfoResponse() : string =
    @"{
        ""success"":1,
        ""return"":{
            ""funds"":{
                ""usd"":0,
                ""btc"":0,
                ""ltc"":0,
                ""nmc"":0,
                ""rur"":0,
                ""eur"":0,
                ""nvc"":0,
                ""trc"":0,
                ""ppc"":0,
                ""ftc"":0,
                ""cnc"":0
            },
            ""rights"":{
                ""info"":1,
                ""trade"":1,
                ""withdraw"":1
            },
            ""transaction_count"":0,
            ""open_orders"":0,
            ""server_time"":1374134297
        }
    }"

let transHistoryResponse() : string =
    @"{
        ""success"":1,
        ""return"":{
	        ""1081672"":{
		        ""type"":1,
		        ""amount"":1.50000000,
		        ""currency"":""BTC"",
		        ""desc"":""BTC Payment"",
		        ""status"":2,
		        ""timestamp"":1342448420
	        }
        }
    }"

let tradeHistoryResponse() : string =
    @"{
	    ""success"":1,
	    ""return"":{
		    ""166830"":{
			    ""pair"":""btc_usd"",
			    ""type"":""sell"",
			    ""amount"":1,
			    ""rate"":1,
			    ""order_id"":343148,
			    ""is_your_order"":1,
			    ""timestamp"":1342445793
		    }
	    }
    }"

let orderListResponse() : string =
    @"{
	    ""success"":1,
	    ""return"":{
		    ""343152"":{
			    ""pair"":""btc_usd"",
			    ""type"":""sell"",
			    ""amount"":1.00000001,
			    ""rate"":3.05000000,
			    ""timestamp_created"":1342448420,
			    ""status"":0
		    }
	    }
    }"

let tradeResponse() : string =
    @"{
	    ""success"":1,
	    ""return"":{
		    ""received"":0.1,
		    ""remains"":0,
		    ""order_id"":0,
		    ""funds"":{
                ""usd"":0,
                ""btc"":0,
                ""ltc"":0,
                ""nmc"":0,
                ""rur"":0,
                ""eur"":0,
                ""nvc"":0,
                ""trc"":0,
                ""ppc"":0,
                ""ftc"":0,
                ""cnc"":0
            }
	    }
    }"

let cancelOrderResponse() : string =
    @"{
	    ""success"":1,
	    ""return"":{
		    ""order_id"":343154,
		    ""funds"":{
                ""usd"":0,
                ""btc"":0,
                ""ltc"":0,
                ""nmc"":0,
                ""rur"":0,
                ""eur"":0,
                ""nvc"":0,
                ""trc"":0,
                ""ppc"":0,
                ""ftc"":0,
                ""cnc"":0
            }
	    }
    }"

let mockRequestHandler (url: string) (key: string) (secret: string) parameters : string =
    let rec getMethod parameters =
        match parameters with
            | [] -> failwith "Required method parameter not found"
            | (field, value) :: tail when field = "method" -> value
            | _ :: tail -> getMethod tail

    let methodName = getMethod parameters

    match methodName with
        | "getInfo" -> getInfoResponse()
        | "TransHistory" -> transHistoryResponse()
        | "TradeHistory" -> tradeHistoryResponse()
        | "OrderList" -> orderListResponse()
        | "Trade" -> tradeResponse()
        | "CancelOrder" -> cancelOrderResponse()
        | _ -> failwith("Unknown method parameter value: " + methodName)