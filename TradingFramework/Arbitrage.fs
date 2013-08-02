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

let createGraph (getInfo: unit -> Info) (getPriceQuotes: Pair list -> (Pair * Quote) list) : Graph =
    let info = getInfo()

    let ticker = getPriceQuotes([for (pair, _) in info.pairs do yield pair ])

    // Join ticker and info on pair
    let graphInfo = [ for (pair, quote) in ticker do
                        match List.tryFind (fun (x, _) -> x = pair) info.pairs with
                            | Some(pair, pairInfo) -> yield (pair, quote, pairInfo)
                            | None -> () ]

    let edges = [ for (pair, quote, pairInfo) in graphInfo do
                        yield {
                            transactionFee = pairInfo.fee;
                            ask = quote.buy;
                            sell = quote.sell;
                            currencyPair = pair
                        } ] 

    let currencies: Currency array = Enum.GetValues(typedefof<Currency>) :?> Currency array;

    { 
        vertices = [ for currency in currencies do
                        yield {
                            currency = currency;
                            edges = [ for edge in edges do
                                        match edge.currencyPair with
                                            | (x, y) when x = currency || y = currency -> yield edge
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