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

    open System
    open Currency

    type Quote = {
        high: Decimal;
        low: Decimal;
        average: Decimal;
        tradingVolume: Decimal;
        tradingVolumeInCurrency: Decimal;
        lastTransactionPrice: Decimal;
        buy: Decimal;
        sell: Decimal;
        updated: int64
    }

    type InfoPair = {
        decimalPlaces: int;
        minPrice: Decimal;
        maxPrice: Decimal;
        minAmount: Decimal;
        hidden: int;
        fee: Decimal
    }

    type Info = {
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
        tradeType: string;
        price: Decimal;
        amount: Decimal;
        tid: int;
        timestamp: int64
    }

    val public getPriceQuotesWithCustomDownloader: (string -> string) -> Pair list -> (Pair * Quote) list

    val public getPriceQuotes: Pair list -> (Pair * Quote) list

    val public getInfoWithCustomDownloader: (string -> string) -> Info

    val public getInfo: unit -> Info

    val public getDepthWithCustomDownloader: (string -> string) -> Pair list -> (Pair * Depth) list

    val public getDepth: Pair list -> (Pair * Depth) list

    //val public getDepthWithLimit: Pair list -> int -> (Pair * Depth) list

    val public getRecentTradesWithCustomDownloader: (string -> string) -> Pair list -> (Pair * (RecentTrade list)) list

    val public getRecentTrades: Pair list -> (Pair * (RecentTrade list)) list
