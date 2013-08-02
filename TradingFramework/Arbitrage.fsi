module Arbitrage

open System

open BtceApiFramework.Currency
open BtceApiFramework.PublicBtceApi

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

val createGraph : (unit -> Info) -> (Pair list -> (Pair * Quote) list) -> Graph
