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

    val public isValidCurrencyPair: Pair -> bool

    val public getCurrency: string -> Currency

    val public getCurrencyPair: string -> Pair

    val public isValidStringPair: string -> bool

    val public currencyPairToString: Pair -> string