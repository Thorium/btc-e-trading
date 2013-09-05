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

module BackTesting

open System.Net
open System

open BtceApiFramework

type Record = (Currency.Pair * PublicBtceApi.Quote)

let diff (lhs: PublicBtceApi.Quote) (rhs: PublicBtceApi.Quote) (apply: Decimal -> Decimal -> Decimal) : PublicBtceApi.Quote =
    {
        high = apply lhs.high rhs.high;
        low = apply lhs.low rhs.low;
        average = apply lhs.average rhs.average;
        tradingVolume = apply lhs.tradingVolume rhs.tradingVolume;
        tradingVolumeInCurrency = apply lhs.tradingVolumeInCurrency rhs.tradingVolumeInCurrency;
        lastTransactionPrice = apply lhs.lastTransactionPrice rhs.lastTransactionPrice;
        buy = apply lhs.buy rhs.buy;
        sell = apply lhs.sell rhs.sell;
        updated = (int64)0;
    }

(*
    Very rudimental algorithm to fill in missing data in the backtesting historic data.
    Creates a straight line from one point to the other
*)
let generateIntermediateValues (emptyPlaces: int) (precedingRecord: Record) (followingRecord: Record) : Record list =
    let (x, precedingQuote) = precedingRecord
    let (z, followingQuote) = followingRecord

    
    let quoteDifference = diff followingQuote precedingQuote (fun x y -> (x - y) / new Decimal(emptyPlaces) * 1)
    
    [(x, quoteDifference)]

let parseLine (line: string) =
    let datetime = DateTime.Parse(line.Substring(0, 19))
    let json = line.Substring(20)

    if json.Length = 0 || not <| json.StartsWith("{") then
        None
    else
        let randomPair = (Currency.Currency.BTC, Currency.Currency.USD)

        let data = BtceApiFramework.PublicBtceApi.getPriceQuotesWithCustomDownloader (fun x -> json) [randomPair] 

        Some(data)

let readHistoricTickerData (file: string) = seq {
    use sr = new System.IO.StreamReader(file)
    while not sr.EndOfStream do
        yield parseLine(sr.ReadLine())
}