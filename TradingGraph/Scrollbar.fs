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

namespace TradingGraph

module Scrollbar =

    open IScrollbar
    open GraphFunctions

    open System
    open System.Drawing
    open System.Drawing.Drawing2D

    type Scrollbar() =
        let scrollbarMovedEvent = new Event<ScrollbarMovedDelegate, EventArgs>()

        interface IScrollbar with 
            member this.Draw (graphics:Graphics) leftMostRecord numberOfRecords (width, height) candleWidth candleLeftMargin =
                graphics.SmoothingMode <- SmoothingMode.AntiAlias

                let numberOfRecordsDisplayed = getNumberOfRecordsCanBeDisplayed width candleWidth candleLeftMargin

                let totalRecordsCanBeDisplayed = numberOfRecords + numberOfRecordsDisplayed - 1

                let totalWidth = (candleWidth + candleLeftMargin) * totalRecordsCanBeDisplayed

                let ratio = float width / float totalWidth

                let scrollbarWidth = width - 20

                let r = float scrollbarWidth / float totalRecordsCanBeDisplayed

                let scrollerWidth = int (float scrollbarWidth * ratio)

                let leftMostRecord = int (float leftMostRecord * r)

                use rect = createRoundedRectangle 10 (height - 14) scrollbarWidth 8 5
                use brush = new SolidBrush(Color.DarkGray)
                graphics.FillPath(brush, rect)

                use rect = createRoundedRectangle (leftMostRecord + 10) (height - 14) scrollerWidth 8 5
                use brush = new SolidBrush(Color.Black)
                graphics.FillPath(brush, rect)

                graphics.SmoothingMode <- SmoothingMode.Default

            member this.ViewMoved x =
                ()

            member this.MouseDown () =
                ()

            member this.MouseMove () =
                ()

            member this.MouseUp () =
                ()

            member this.ScrollerBoundingBox () =
                RectangleF(10.0f,10.0f,10.0f,10.0f)

            member this.ScrollbarBoundingBox () =
                RectangleF(10.0f,10.0f,10.0f,10.0f)

            [<CLIEvent>]
            member this.ScrollbarMovedEvent = scrollbarMovedEvent.Publish