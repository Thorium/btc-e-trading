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

module Currency =

    type Currency = 
        | USD = 0 // US Dollar
        | BTC = 1 // Bitcoin
        | LTC = 2 // Litecoin
        | NMC = 3 // Namecoin
        | RUR = 4 // Russian Ruble
        | EUR = 5 // Euro    
        | NVC = 6 // Novacoin
        | TRC = 7 // Terra
        | PPC = 8 // PPCoin (Peer-to-Peer Coin)
        | FTC = 9 // Feathercoin
        | CNC = 10 // CNCoin (China coin)
        | XPM = 11 // PrimeCoin

    type Pair = Currency * Currency

    let isValidCurrencyPair (pair: Pair) : bool =
        let (left, right) = pair
        left <> right

    let getCurrency (currency: string) : Currency =
        match currency.ToUpper() with 
            | "USD" -> Currency.USD
            | "BTC" -> Currency.BTC
            | "LTC" -> Currency.LTC
            | "NMC" -> Currency.NMC
            | "RUR" -> Currency.RUR
            | "EUR" -> Currency.EUR
            | "NVC" -> Currency.NVC
            | "TRC" -> Currency.TRC
            | "PPC" -> Currency.PPC
            | "FTC" -> Currency.FTC
            | "CNC" -> Currency.CNC
            | "XPM" -> Currency.XPM
            | _ -> failwith("Unknown currency " + currency)

    let getCurrencyPair (pair: string) : Pair =
        if pair.Length <> 7 then failwith("Pair must be exactly 7 characters long; pair was " + pair.Length.ToString() + " characters long.")

        let left = getCurrency(pair.Substring(0, 3))

        if pair.Chars(3) <> '_' then failwith "Pair must have an underscore between the two currencies."

        let right = getCurrency(pair.Substring(4, 3))

        if left = right then failwith "Pair must consist of two different currencies."

        (left, right)

    let isValidStringPair (pair: string) : bool =
        isValidCurrencyPair(getCurrencyPair pair)

    let currencyPairToString (pair: Pair) : string =
        let (left, right) = pair
        left.ToString().ToLower() + "_" + right.ToString().ToLower()