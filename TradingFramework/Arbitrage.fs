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

module Arbitrage

open BtceApiFramework.Currency
open BtceApiFramework.PublicBtceApi

open System

type Vertex = { 
    currency: Currency
}

type Edge = { 
    transactionFee: Decimal;
    ask: Decimal;
    sell: Decimal;
    currencyPair: Pair;
    vertices: Vertex * Vertex;
}

type AdjacencyList = {
    vertex: Vertex;
    edges: Edge list
}

type Graph = { adjacencyLists: AdjacencyList list }

let createGraph (getInfo: unit -> Info) (getPriceQuotes: Pair list -> (Pair * Quote) list) : Graph =
    let info = getInfo()

    let ticker = getPriceQuotes([for (pair, _) in info.pairs do yield pair ])

    // Join ticker and info on pair
    let graphInfo = [ for (pair, quote) in ticker do
                        match List.tryFind (fun (x, _) -> x = pair) info.pairs with
                            | Some(pair, pairInfo) -> yield (pair, quote, pairInfo)
                            | None -> () ]

    let currencies: Currency array = Enum.GetValues(typedefof<Currency>) :?> Currency array;

    let vertices = [ for currency in currencies do yield { currency = currency; } ]

    let findVertexWithCurrency (currency: Currency) : Vertex = 
        List.find (fun x -> x.currency = currency) vertices

    let edges = [ for (pair, quote, pairInfo) in graphInfo do
                        let (left, right) = pair
                        yield {
                            transactionFee = pairInfo.fee;
                            ask = quote.buy;
                            sell = quote.sell;
                            currencyPair = pair;
                            vertices = (findVertexWithCurrency(left), findVertexWithCurrency(right))
                        } ]

    { 
        adjacencyLists = [ for vertex in vertices do
                            yield {
                                vertex = vertex; 
                                edges = [ for edge in edges do
                                            match edge.currencyPair with
                                                | (x, y) when x = vertex.currency || y = vertex.currency -> yield edge
                                                | _ -> () ]
                            } ]
    }

// Find shortest paths - shortest path = highest amount of money made; shortest path to the vertex you start from

let findShortestPath (from: Vertex) : Vertex list =
    []

(*
 a*

paths

 

 start with the edge with the lowest cost
    keep measure of cost of each path




*)