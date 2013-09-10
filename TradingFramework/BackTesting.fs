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
        let (_, precedingQuote) = precedingRecord
        let (currencyPair, followingQuote) = followingRecord
        yield [ for i in 1..emptyPlaces do
                    let operation lastValue firstValue = 
                        if firstValue < lastValue then
                            firstValue + ((lastValue - firstValue) / new Decimal(emptyPlaces + 1) * new Decimal(i))
                        else
                            firstValue - ((firstValue - lastValue) / new Decimal(emptyPlaces + 1) * new Decimal(i))

                    yield (currencyPair, operate followingQuote precedingQuote operation) ] ]

type ReadLine = unit -> string option

let parseLine (line: string) =
    let lastDateTimeCharacterPosition = 19
    let firstJsonCharacterPosition = lastDateTimeCharacterPosition + 1

    if line.Length < firstJsonCharacterPosition + 1 then
        None
    else
        let datetime = DateTime.Parse(line.Substring(0, lastDateTimeCharacterPosition))
        let json = line.Substring(firstJsonCharacterPosition)

        if json.Length = 0 || not <| json.StartsWith("{") then
            None
        else
            let randomPair = (Currency.Currency.BTC, Currency.Currency.USD)

            let data = BtceApiFramework.PublicBtceApi.getPriceQuotesWithCustomDownloader (fun x -> json) [randomPair] 

            Some(data)

let generateMissingData (readLine: ReadLine) previousRecord = 
    let rec readLineWithMissingData (readLine: ReadLine) i =
        match readLine() with
            | None -> None
            | Some(line) ->
                match parseLine(line) with
                    | None -> readLineWithMissingData readLine (i + 1)
                    | Some(x) -> 
                        Some(generateIntermediateValuesForLists i previousRecord x, x)            

    readLineWithMissingData readLine 1
    
let readHistoricTickerData (readLine: ReadLine) (apply: Record list -> unit) =
    let rec readLines (streamReader: ReadLine) (previousRecord: Record list option) = 
        match readLine() with 
            | None -> ()
            | Some(line) -> 
                match parseLine(line) with
                    | None when previousRecord.IsSome -> 
                        let missingData = generateMissingData streamReader previousRecord.Value
                        if missingData.IsNone then
                            readLines streamReader previousRecord
                        else
                            let (generatedMissingRecords, lastRecord) = missingData.Value
                            for generatedMissingRecord in generatedMissingRecords do
                                apply(generatedMissingRecord)
                            apply(lastRecord)
                            readLines streamReader (Some(lastRecord))
                    | None -> 
                        readLines streamReader None
                    | Some(record) ->
                        apply(record)
                        readLines streamReader (Some(record))
    
    readLines readLine None