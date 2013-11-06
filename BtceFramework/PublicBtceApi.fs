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

namespace BtceApiFramework

module PublicBtceApi =

    open System.Net
    open System
    open Newtonsoft.Json

    open Currency

    type Quote = {
        high: Decimal;
        low: Decimal;
        [<field: JsonProperty(PropertyName="avg")>] 
        average: Decimal;
        [<field: JsonProperty(PropertyName="vol")>] 
        tradingVolume: Decimal;
        [<field: JsonProperty(PropertyName="vol_cur")>] 
        tradingVolumeInCurrency: Decimal;
        [<field: JsonProperty(PropertyName="last")>] 
        lastTransactionPrice: Decimal;
        buy: Decimal;
        sell: Decimal;
        updated: int64
    }

    type InfoPair = {
        [<field: JsonProperty(PropertyName="decimal_places")>] 
        decimalPlaces: int;
        [<field: JsonProperty(PropertyName="min_price")>] 
        minPrice: Decimal;
        [<field: JsonProperty(PropertyName="max_price")>] 
        maxPrice: Decimal;
        [<field: JsonProperty(PropertyName="min_amount")>] 
        minAmount: Decimal;
        hidden: int;
        fee: Decimal
    }

    type Info = {
        [<field: JsonProperty(PropertyName="server_time")>] 
        serverTime: int64;
        pairs: (Pair * InfoPair) list
    }

    type DepthItem = {
        price: Decimal;
        amount: Decimal
    }

    type Depth = {
        asks: DepthItem list;
        bids: DepthItem list
    }

    type RecentTrade = {
        [<field: JsonProperty(PropertyName="type")>] 
        tradeType: string;
        price: Decimal;
        amount: Decimal;
        tid: int;
        timestamp: int64
    }

    let getParameters (pairs: Pair list) : string = 
        let stringPairs = List.map (fun x -> currencyPairToString(x)) pairs
        List.reduce (fun x y -> x + "-" + y) stringPairs

    let parseQuotes (jsonObject: Newtonsoft.Json.Linq.JToken) : Quote =
        JsonConvert.DeserializeObject<Quote>(jsonObject.ToString())

    let getPriceQuotesWithCustomDownloader (customDownloader: string -> string) (pairs: Pair list) : (Pair * Quote) list =
        let response = customDownloader("https://btc-e.com/api/3/ticker/" + getParameters(pairs))
        let jsonObject = Newtonsoft.Json.Linq.JObject.Parse(response)
        [ for child in jsonObject
            do 
                let childProperty: Linq.JProperty = child :?> Linq.JProperty
                let pair = childProperty.Name
                yield (getCurrencyPair(pair), parseQuotes jsonObject.[pair]) ]

    let getPriceQuotes (pairs: Pair list) : (Pair * Quote) list = 
        let client = new WebClient()
        getPriceQuotesWithCustomDownloader client.DownloadString pairs
    
    let parseInfoPair (jsonObject: Newtonsoft.Json.Linq.JToken) : InfoPair =
        JsonConvert.DeserializeObject<InfoPair>(jsonObject.ToString())

    let parseInfoPairs (jsonObject: Newtonsoft.Json.Linq.JToken) : (Pair * InfoPair) list =
        [ for child in jsonObject
            do 
                let childProperty: Linq.JProperty = child :?> Linq.JProperty
                let pair = childProperty.Name
                let child = parseInfoPair(jsonObject.[pair])
                yield (getCurrencyPair(pair), child) ]

    let parseInfo (jsonObject: Newtonsoft.Json.Linq.JToken) : Info =
        {
            serverTime  = (int64)jsonObject.["server_time"];
            pairs       = parseInfoPairs jsonObject.["pairs"]
        }

    let getInfoWithCustomDownloader (customDownloader: string -> string) : Info =
        let response = customDownloader("https://btc-e.com/api/3/info")
        let jsonObject = Newtonsoft.Json.Linq.JObject.Parse(response)
        parseInfo jsonObject

    let getInfo() : Info = 
        let client = new WebClient()
        getInfoWithCustomDownloader client.DownloadString

    let parseDepthItem (jsonObject: Newtonsoft.Json.Linq.JToken) : DepthItem =
        {
            price = jsonObject.[0].ToObject<Decimal>();
            amount = jsonObject.[1].ToObject<Decimal>()
        }

    let parseDepth (jsonObject: Newtonsoft.Json.Linq.JToken) : Depth =
        {
            asks = [ for child in jsonObject.["asks"] do yield parseDepthItem child ];
            bids = [ for child in jsonObject.["bids"] do yield parseDepthItem child ]
        }

    let getDepthWithCustomDownloader (customDownloader: string -> string) (pairs: Pair list) : (Pair * Depth) list =
        let response = customDownloader("https://btc-e.com/api/3/depth/" + getParameters(pairs))
        let jsonObject = Newtonsoft.Json.Linq.JObject.Parse(response)
        [ for child in jsonObject
            do 
                let childProperty: Linq.JProperty = child :?> Linq.JProperty
                let pair = childProperty.Name
                yield (getCurrencyPair(pair), parseDepth jsonObject.[pair]) ]

    let getDepth (pairs: Pair list) : (Pair * Depth) list = 
        let client = new WebClient()
        getDepthWithCustomDownloader client.DownloadString pairs

    let parseRecentTrade (jsonObject: Newtonsoft.Json.Linq.JToken) : RecentTrade =
        JsonConvert.DeserializeObject<RecentTrade>(jsonObject.ToString())

    let parseRecentTrades (jsonObject: Newtonsoft.Json.Linq.JToken) : RecentTrade list =
        [ for child in jsonObject do 
            yield parseRecentTrade child ]

    let getRecentTradesWithCustomDownloader (customDownloader: string -> string) (pairs: Pair list) : (Pair * (RecentTrade list)) list =
        let response = customDownloader("https://btc-e.com/api/3/trades/" + getParameters(pairs))
        let jsonObject = Newtonsoft.Json.Linq.JObject.Parse(response)
        [ for child in jsonObject
            do 
                let childProperty: Linq.JProperty = child :?> Linq.JProperty
                let pair = childProperty.Name
                yield (getCurrencyPair(pair), parseRecentTrades jsonObject.[pair]) ]

    let getRecentTrades (pairs: Pair list) : (Pair * (RecentTrade list)) list = 
        let client = new WebClient()
        getRecentTradesWithCustomDownloader client.DownloadString pairs