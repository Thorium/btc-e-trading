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

let operate (lhs: PublicBtceApi.Quote) (rhs: PublicBtceApi.Quote) (operation: Decimal -> Decimal -> Decimal) : PublicBtceApi.Quote =
    {
        high = operation lhs.high rhs.high;
        low = operation lhs.low rhs.low;
        average = operation lhs.average rhs.average;
        tradingVolume = operation lhs.tradingVolume rhs.tradingVolume;
        tradingVolumeInCurrency = operation lhs.tradingVolumeInCurrency rhs.tradingVolumeInCurrency;
        lastTransactionPrice = operation lhs.lastTransactionPrice rhs.lastTransactionPrice;
        buy = operation lhs.buy rhs.buy;
        sell = operation lhs.sell rhs.sell;
        updated = (int64)0;
    }

(*
    Very rudimental algorithm to fill in missing data in the backtesting historic data.
    Creates a straight line from one point to the other
*)
let generateIntermediateValues (emptyPlaces: int) (precedingRecord: Record) (followingRecord: Record) : Record list =
    let (_, precedingQuote) = precedingRecord
    let (currencyPair, followingQuote) = followingRecord

    [ for i in 1..emptyPlaces do
        let operation lastValue firstValue = 
            if firstValue < lastValue then
                firstValue + ((lastValue - firstValue) / new Decimal(emptyPlaces + 1) * new Decimal(i))
            else
                firstValue - ((firstValue - lastValue) / new Decimal(emptyPlaces + 1) * new Decimal(i))

        yield (currencyPair, operate followingQuote precedingQuote operation) ]

let generateIntermediateValuesForLists (emptyPlaces: int) (precedingRecordList: Record list) (followingRecordList: Record list) : (Record list) list =
    [ for (precedingRecord, followingRecord) in Seq.zip precedingRecordList followingRecordList do
        yield generateIntermediateValues emptyPlaces precedingRecord followingRecord ]

let parseLine (line: string) =
    let datetime = DateTime.Parse(line.Substring(0, 19))
    let json = line.Substring(20)

    if json.Length = 0 || not <| json.StartsWith("{") then
        None
    else
        let randomPair = (Currency.Currency.BTC, Currency.Currency.USD)

        let data = BtceApiFramework.PublicBtceApi.getPriceQuotesWithCustomDownloader (fun x -> json) [randomPair] 

        Some(data)

let generateMissingData (streamReader: System.IO.StreamReader) previousRecord = seq {
    let i = ref 1
    while not streamReader.EndOfStream do
        match parseLine(streamReader.ReadLine()) with
            | None -> i.Value <- !i + 1
            | Some(x) -> 
                yield! generateIntermediateValuesForLists !i previousRecord x
                yield x
}

let readHistoricTickerData (file: string) = seq {
    let (previousRecord: (Record list) option ref) = ref None
    use streamReader = new System.IO.StreamReader(file)
    while not streamReader.EndOfStream do
        match parseLine(streamReader.ReadLine()) with
            | None when (!previousRecord).IsSome -> yield! generateMissingData streamReader (!previousRecord).Value
            | None -> ()
            | Some(x) -> 
                previousRecord.Value <- Some(x)
                yield x
}