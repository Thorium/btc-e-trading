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

module TestCurrency

open NUnit.Framework

open BtceApiFramework.Currency

[<TestFixture>]
type TestCurrency() = class

    [<Test>]
    member self.getInvalidCurrencyPairFromString() = 
        Assert.Throws(fun() -> getCurrencyPair "btc_ usd" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "btc_btc" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "usd_usd" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "usd_asc" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "us_ltc" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "usd_lt" |> ignore) |> ignore
        Assert.Throws(fun() -> getCurrencyPair "usd ltc" |> ignore) |> ignore

    [<Test>]
    member self.getCurrencyPairFromString() = 
        let (left, right) = getCurrencyPair "btc_usd"
        Assert.AreEqual(left, Currency.BTC)
        Assert.AreEqual(right, Currency.USD)

        let (left, right) = getCurrencyPair "eur_ltc"
        Assert.AreEqual(left, Currency.EUR)
        Assert.AreEqual(right, Currency.LTC)
end