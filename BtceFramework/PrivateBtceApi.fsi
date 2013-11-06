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

module PrivateBtceApi =

    open System
    open Newtonsoft.Json

    open Currency

    type OrderBy =
        | Ascending
        | Descending

    type TradeType =
        | Buy
        | Sell

    type Parameter =
        | From      of int
        | Count     of int
        | FromId    of int
        | EndId     of int
        | Order     of OrderBy
        | Since     of int64
        | End       of int64
        | Pair      of (Currency * Currency)
        | Type      of TradeType
        | Rate      of Decimal
        | Amount    of Decimal
        | OrderId   of int
        | Active    of bool

    type ExpectedParameter = {
        required: bool;
        parameterName: string
    }

    type ValidationResult = 
        | Success
        | Failure of string

    val public validateParameters: ExpectedParameter list -> Parameter list -> ValidationResult

    val public parameterToString: Parameter -> string

    val public parameterValueToString: Parameter -> string

    type Response<'T> =
        | Success of 'T
        | Error of string

    type Funds = {
        usd: Decimal;
        btc: Decimal;
        ltc: Decimal;
        nmc: Decimal;
        rur: Decimal;
        eur: Decimal;
        nvc: Decimal;
        trc: Decimal;
        ppc: Decimal;
        ftc: Decimal;
        cnc: Decimal
    }

    type Rights = { 
        info: int;
        trade: int;
        withdraw: int 
    }

    type AccountInformation = {
        funds: Funds;
        rights: Rights;
        transactionCount: int;
        openOrders: int;
        serverTime: int64
    }

    type Transaction = {
        [<field: JsonProperty(PropertyName="type")>] 
        transactionType: int;
        amount: Decimal;
        currency: string; // Currency
        desc: string;
        status: int;
        timestamp: int64
    }

    type TransactionHistory = {
        transactions: (int * Transaction) list
    }

    type PastTrade = {
        pair: string; // Currency * Currency
        [<field: JsonProperty(PropertyName="type")>] 
        tradeType: string; // TradeType
        amount: Decimal;
        rate: Decimal;
        [<field: JsonProperty(PropertyName="order_id")>] 
        orderId: int;
        [<field: JsonProperty(PropertyName="is_your_order")>] 
        isYourOrder: int; // bool
        timestamp: int64
    }

    type TradeHistory = {
        trades: (int * PastTrade) list
    }

    type Order = {
        pair: string;
        [<field: JsonProperty(PropertyName="type")>] 
        orderType: string;
        amount: Decimal;
        rate: Decimal;
        [<field: JsonProperty(PropertyName="timestamp_created")>] 
        timestampCreated: int64;
        status: int
    }

    type OrderList = {
        orders: (int * Order) list
    }

    type Trade = {
        received: Decimal;
        remains: Decimal;
        [<field: JsonProperty(PropertyName="order_id")>] 
        orderId: int;
        funds: Funds
    }

    type CancelledOrder = {
        [<field: JsonProperty(PropertyName="order_id")>] 
        orderId: int;
        funds: Funds
    }

    /// <summary>Gets information about the user's account, this includes their balance, number of transactions,
    /// number of open orders, and the server time. Provides the ability to specify a custom request handler.
    /// Having a custom request handler allows for you to create custom responses, this can be used for both 
    /// testing and creating a "simulation" server.</summary>
    /// <param name="requestHandler">Handler; takes url, key, secret, parameters; returns json response</param>
    /// <param name="key">API key.</param>
    /// <param name="secret">API secret.</param>
    /// <returns>User's account information.</returns>
    val public getAccountInformationWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Response<AccountInformation>

    /// <summary>Gets information about the user's account, this includes their balance, number of transactions,
    /// number of open orders, and the server time.</summary>
    /// <param name="key">API key.</param>
    /// <param name="secret">API secret.</param>
    /// <returns>User's account information.</returns>
    val public getAccountInformation: (string -> string -> Response<AccountInformation>)


    val public getTransactionHistoryWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Parameter list -> Response<TransactionHistory>

    val public getTransactionHistory: (string -> string -> Parameter list -> Response<TransactionHistory>)


    val public getTradeHistoryWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Parameter list -> Response<TradeHistory>

    val public getTradeHistory: (string -> string -> Parameter list -> Response<TradeHistory>)


    val public getOrderListWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Parameter list -> Response<OrderList>

    val public getOrderList: (string -> string -> Parameter list -> Response<OrderList>)


    val public tradeWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Parameter list -> Response<Trade>

    val public trade: (string -> string -> Parameter list -> Response<Trade>)


    val public cancelOrderWithCustomRequestHandler: (string -> string -> string -> (string * string) list -> string) -> string -> string -> Parameter list -> Response<CancelledOrder>

    val public cancelOrder: (string -> string -> Parameter list -> Response<CancelledOrder>)