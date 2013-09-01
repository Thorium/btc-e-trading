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

let parseLine (line: string) =
    let datetime = DateTime.Parse(line.Substring(0, 19))
    let json = line.Substring(20)

    if json.Length = 0 then
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