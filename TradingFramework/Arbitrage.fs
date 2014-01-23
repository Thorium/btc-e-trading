﻿(*
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

namespace TradingFramework

module Arbitrage =

    open BtceApiFramework.Currency
    open BtceApiFramework.PublicBtceApi

    open System

    type Vertex = { 
        currency: Currency
    }

    type EdgeDirection =
        | Left
        | Right

    type PairTicker = {
        transactionFee: Decimal
        pair: Pair
        sell: Decimal
        buy: Decimal
    }

    type Edge = { 
        direction: EdgeDirection
        exchangeRate: Decimal
        pairTicker: PairTicker
        currencyPair: Pair
        vertices: Vertex * Vertex
    }

    type AdjacencyList = {
        vertex: Vertex
        edges: Edge list
    }

    type Graph = { adjacencyLists: AdjacencyList list }

    let getExchangeRate fromCurrency info =
        match info.pair with
        | (left, right) when left = fromCurrency -> 
            info.sell - (info.sell * (info.transactionFee / 100m))
        | (left, right) when right = fromCurrency -> 
            (1m / info.buy) - ((1m / info.buy) * (info.transactionFee / 100m))
        | _ -> failwith ("From and to currencies did not match currency pair: " + currencyPairToString info.pair)

    let createEdge direction pair pairTicker vertices =
        let findVertexWithCurrency (currency: Currency) = 
            List.find (fun x -> x.currency = currency) vertices

        let left, right = pair

        {
            direction = direction
            exchangeRate = getExchangeRate (if direction = EdgeDirection.Left then left else right) pairTicker
            pairTicker = pairTicker
            currencyPair = pair
            vertices = (findVertexWithCurrency left, findVertexWithCurrency right)
        }

    let createGraph getInfo (getPriceQuotes: Pair list -> (Pair * Quote) list) =
        let info = getInfo()

        let ticker = getPriceQuotes([for (pair, _) in info.pairs do yield pair ])

        // Join ticker and info on pair
        let graphInfo = [ for (pair, quote) in ticker do
                            match List.tryFind (fun (x, _) -> x = pair) info.pairs with
                                | Some(pair, pairInfo) -> yield (pair, quote, pairInfo)
                                | None -> () ]

        let currencies: Currency array = Enum.GetValues(typedefof<Currency>) :?> Currency array

        let vertices = [ for currency in currencies do yield { currency = currency; } ]

        let edges = [ for (pair, quote, pairInfo) in graphInfo do
                            let (left, right) = pair

                            let pairTicker = {
                                transactionFee = pairInfo.fee
                                pair = pair
                                sell = quote.sell
                                buy = quote.buy
                            }

                            yield createEdge EdgeDirection.Left pair pairTicker vertices
                            yield createEdge EdgeDirection.Right pair pairTicker vertices ]

        { 
            adjacencyLists = [ for vertex in vertices do
                                yield {
                                    vertex = vertex; 
                                    edges = [ for edge in edges do
                                                match edge.currencyPair with
                                                    | (x, y) when edge.direction = EdgeDirection.Left && x = vertex.currency 
                                                        -> yield edge
                                                    | (x, y) when edge.direction = EdgeDirection.Right && y = vertex.currency 
                                                        -> yield edge
                                                    | _ -> () ]
                                } ]
        }

    let pathProfit path =
        List.fold (fun rate edge -> rate * edge.exchangeRate) 1m path

    let adjacencyListForVertex vertex graph =
        List.find (fun x -> x.vertex = vertex) graph.adjacencyLists

    let adjacencyListForCurrency currency graph =
        List.find (fun x -> x.vertex.currency = currency) graph.adjacencyLists

    let paths from graph limit = 
        let finalVertex = from.vertex

        let rec processList from edges pairsVisited edgesVisited = 
            let mutable paths: Edge list list = []

            for edge in edges do
                if not <| List.exists (fun x -> x = edge.currencyPair) pairsVisited then
                    let toVertex = match edge.vertices with
                                        | (x, y) when y = from -> x
                                        | (x, y) when x = from -> y
                                        | _ -> failwith ("Expected edge to contain vertex: " + from.currency.ToString())

                    let adjacencyList = (adjacencyListForVertex toVertex graph)

                    if finalVertex = toVertex then
                        paths <- (edge :: edgesVisited) :: paths
                    else if edgesVisited.Length + 1 < limit then
                        let pairsVisited = edge.currencyPair :: pairsVisited
                        let edgesVisited = edge :: edgesVisited
                        paths <- (processList adjacencyList.vertex adjacencyList.edges pairsVisited edgesVisited) @ paths
              
            paths

        processList from.vertex from.edges [] []