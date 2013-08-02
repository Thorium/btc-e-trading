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

val public createGraph: (unit -> Info) -> (Pair list -> (Pair * Quote) list) -> Graph