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

module TestPrivateApi

open NUnit.Framework

open System

open BtceApiFramework.PrivateBtceApi
open BtceApiFramework.Currency

open MockRequestHandler

let assertValidFunds (funds: Funds) =
    Assert.That(funds.usd = 0m)
    Assert.That(funds.btc = 0m)
    Assert.That(funds.ltc = 0m)
    Assert.That(funds.nmc = 0m)
    Assert.That(funds.rur = 0m)
    Assert.That(funds.eur = 0m)
    Assert.That(funds.nvc = 0m)
    Assert.That(funds.trc = 0m)
    Assert.That(funds.ppc = 0m)
    Assert.That(funds.ftc = 0m)
    Assert.That(funds.cnc = 0m)

let expectedParameters = [
    { required = true; parameterName = "from" };
    { required = false; parameterName = "count" };
    { required = true; parameterName = "amount" };
    { required = false; parameterName = "since" };
    { required = true; parameterName = "pair" };
]

[<TestFixture>]
type TestPrivateApi() = class

    let key = "testkey"
    let secret = "testsecret"

    [<Test>]
    member self.getAccountInformation() = 
        match getAccountInformationWithCustomRequestHandler mockRequestHandler key secret with
            | Response.Success(response) -> 
                assertValidFunds(response.funds)

                Assert.That(response.rights.info = 1)
                Assert.That(response.rights.trade = 1)
                Assert.That(response.rights.withdraw = 1)

                Assert.That(response.transactionCount = 0)
                Assert.That(response.openOrders = 0)
                Assert.That(response.serverTime = (int64)1374134297)
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.getTransactionHistory() = 
        match getTransactionHistoryWithCustomRequestHandler mockRequestHandler key secret [] with
            | Response.Success(response) -> 
                let (transactionId, transaction) = response.transactions.Head

                Assert.That(transaction.transactionType = 1)
                Assert.That(transaction.amount = 1.5m)
                Assert.That(transaction.currency = "BTC")
                Assert.That(transaction.desc = "BTC Payment")
                Assert.That(transaction.status = 2)
                Assert.That(transaction.timestamp = (int64)1342448420)

                Assert.AreEqual(transactionId, 1081672) |> ignore
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.getTradeHistory() = 
        match getTradeHistoryWithCustomRequestHandler mockRequestHandler key secret [] with
            | Response.Success(response) -> 
                let (tradeId, trade) = response.trades.Head

                Assert.AreEqual(tradeId, 166830) |> ignore

                Assert.That(trade.amount = 1m)
                Assert.That(trade.isYourOrder = 1)
                Assert.That(trade.orderId = 343148)
                Assert.That(trade.pair = "btc_usd")
                Assert.That(trade.rate = 1m)
                Assert.That(trade.timestamp = (int64)1342445793)
                Assert.That(trade.tradeType = "sell")
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.getOrderList() = 
        match getOrderListWithCustomRequestHandler mockRequestHandler key secret [] with
            | Response.Success(response) -> 
                let (orderId, order) = response.orders.Head

                Assert.AreEqual(orderId, 343152) |> ignore

                Assert.That(order.amount = 1.00000001m)
                Assert.That(order.orderType = "sell")
                Assert.That(order.pair = "btc_usd")
                Assert.That(order.rate = 3.05m)
                Assert.That(order.timestampCreated = (int64)1342448420)
                Assert.That(order.status = 0)
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.trade() = 
        let parameters = [
            Pair(Currency.BTC, Currency.LTC);
            Type(TradeType.Buy);
            Rate(1m);
            Amount(1m)
        ]

        match tradeWithCustomRequestHandler mockRequestHandler key secret parameters with
            | Response.Success(response) -> 
                assertValidFunds(response.funds)

                Assert.That(response.orderId = 0)
                Assert.That(response.received = 0.1m)
                Assert.That(response.remains = 0m)
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.cancel() = 
        let parameters = [
            OrderId(123121)
        ]

        match cancelOrderWithCustomRequestHandler mockRequestHandler key secret parameters with
            | Response.Success(info) -> 
                assertValidFunds(info.funds)

                Assert.That(info.orderId = 343154)
            | Error(error) -> Assert.Fail()

    [<Test>]
    member self.validateParameters() = 
        let parameters: Parameter list = [
            From(10);
            Count(50);
            Amount(50.5m);
            Since((int64)67);
            Pair((Currency.BTC, Currency.LTC))
        ]

        let successfulValidation = validateParameters expectedParameters parameters = ValidationResult.Success
        Assert.That(successfulValidation)
        
    [<Test>]
    member self.validateParametersWithDuplicateParameters() = 
        let parameters: Parameter list = [
            From(10);
            Count(50);
            Amount(50.5m);
            Since((int64)67);
            Pair((Currency.BTC, Currency.LTC));
            Count(50)
        ]

        let failedValidation = validateParameters expectedParameters parameters <> ValidationResult.Success
        Assert.That(failedValidation)

    [<Test>]
    member self.validateParametersWithoutRequiredParameter() = 
        let parameters: Parameter list = [
            From(10);
            Count(50);
            Amount(50.5m);
            Since((int64)67)
        ]

        let failedValidation = validateParameters expectedParameters parameters <> ValidationResult.Success
        Assert.That(failedValidation)

    [<Test>]
    member self.validateParametersWithUnexpectedParameter() = 
        let parameters: Parameter list = [
            From(10);
            Count(50);
            Amount(50.5m);
            Since((int64)67);
            Pair((Currency.BTC, Currency.LTC));
            End((int64)77)
        ]

        let failedValidation = validateParameters expectedParameters parameters <> ValidationResult.Success
        Assert.That(failedValidation)
end