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