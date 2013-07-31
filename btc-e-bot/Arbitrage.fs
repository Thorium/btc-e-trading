module Arbitrage

open BtceApiFramework.Currency
open BtceApiFramework.PublicBtceApi

open System

type Edge = { 
    transactionFee: Decimal;
    ask: Decimal;
    sell: Decimal;
    currencyPair: Pair
}

type Vertex = { 
    currency: Currency;
    edges: Edge list 
}

type Graph = { vertices: Vertex list }

// Find shortest paths - shortest path = highest amount of money made; shortest path to the vertx you start from

let findShortestPath (vertex: Vertex) : unit =
    ()