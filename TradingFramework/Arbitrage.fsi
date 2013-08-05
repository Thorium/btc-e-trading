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

open System

open BtceApiFramework.Currency
open BtceApiFramework.PublicBtceApi

type Vertex = { 
    currency: Currency
}

type EdgeDirection =
    | Left
    | Right

type PairTicker = {
    transactionFee: Decimal;
    pair: Pair;
    sell: Decimal;
    ask: Decimal
}

type Edge = { 
    direction: EdgeDirection;
    exchangeRate: Decimal;
    pairTicker: PairTicker;
    currencyPair: Pair;
    vertices: Vertex * Vertex;
}

type AdjacencyList = {
    vertex: Vertex;
    edges: Edge list
}

type Graph = { adjacencyLists: AdjacencyList list }

// Creates a directed graph of currencies. The edges represent an exchange from one currency to another and as such have a direction. 
// Each edge will always be matched with an edge going the other way 
// e.g. if you have 2 vertices, v1 and v2, and an edge e1 going from v1 to v2, 
// then we can guarantee there will also be an edge e2 going from v2 to v1.
val public createGraph: (unit -> Info) -> (Pair list -> (Pair * Quote) list) -> Graph

val public pathProfit: Edge list -> Decimal

val public adjacencyListForCurrency: Currency -> Graph -> AdjacencyList

// Gets all possible cycles starting and ending with a given currency, while not moving between currency pairs more than once.
// If the size of the graph grows we may want to switch to the Bellman-Ford algorithm.
val public paths: AdjacencyList -> Graph -> (Edge list) list