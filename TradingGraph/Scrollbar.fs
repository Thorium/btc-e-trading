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
    open System.Collections.Concurrent
    open System.Drawing
    open System.Drawing.Drawing2D
    open System.Threading

    type Fading =
    | In
    | Out
    | None

    /// Scrollbar that shows movement, fades in on the start of movement and fades out at the end of movement.
    /// The scrollbar can only be viewed by the user kind of "read-only".
    type Scrollbar() =
        let redrawEvent = Event<unit>()

        let mutable isMoving, isMouseDown, displayed = false, false, false

        let mutable alpha = 0

        let fader = new MailboxProcessor<string>(fun inbox ->
            let rec loop timeout fading =
                async { 
                    let fading = match fading with
                    | In -> 
                        let r = if alpha + 40 >= 255 then
                                    alpha <- 255
                                    None
                                else
                                    alpha <- alpha + 40
                                    In
                        redrawEvent.Trigger()
                        r
                    | Out -> 
                        let r = if alpha - 40 <= 0 then
                                    alpha <- 0
                                    displayed <- false
                                    None
                                else
                                    alpha <- alpha - 40
                                    Out
                        redrawEvent.Trigger()
                        r
                    | None -> None
                    
                    let! result = 
                        match fading with
                        | None -> inbox.TryReceive ()
                        | _ -> inbox.TryReceive timeout

                    match result with
                    | Some("fadeIn") -> 
                        displayed <- true
                        do! loop timeout Fading.In
                    | Some("fadeOut") -> 
                        do! loop timeout Fading.Out
                    | _ ->  
                        do! loop timeout fading
                } 
            loop 30 None)

        let checkMovement = new MailboxProcessor<string>(fun inbox ->
            let rec loop timeout =
                async { 
                    let! result = 
                        if isMoving then
                            inbox.TryReceive timeout
                        else
                            inbox.TryReceive ()

                    match result with
                    | Some(message) when message.Equals("viewmoved") -> 
                        isMoving <- true
                    | _ when not isMouseDown -> 
                        fader.Post "fadeOut"
                        isMoving <- false
                    | _ -> ()
                        
                    do! loop timeout
                } 
            loop 75)

        do
            checkMovement.Start()
            fader.Start()
            
        interface IScrollbar with 
            member this.Draw (graphics:Graphics) leftMostRecord numberOfRecords (width, height) candleWidth candleLeftMargin =
                if displayed then

                    let numberOfRecordsDisplayed = getNumberOfRecordsCanBeDisplayed width candleWidth candleLeftMargin

                    let totalRecordsCanBeDisplayed = numberOfRecords + numberOfRecordsDisplayed - 1

                    let totalWidth = (candleWidth + candleLeftMargin) * totalRecordsCanBeDisplayed

                    let ratio = float width / float totalWidth

                    let scrollbarWidth = width

                    let r = float scrollbarWidth / float totalRecordsCanBeDisplayed

                    let scrollerWidth = int (float scrollbarWidth * ratio)

                    let leftMostRecord = int (float leftMostRecord * r)

                    let rect = Rectangle(0, height - 4, scrollbarWidth, 4)
                    use brush = new SolidBrush(Color.Black)
                    graphics.FillRectangle(brush, rect)

                    graphics.SmoothingMode <- SmoothingMode.AntiAlias

                    use rect = createRoundedRectangle leftMostRecord (height - 4) scrollerWidth 4 2
                    use brush = new SolidBrush(Color.FromArgb(alpha, Color.DarkGray))
                    graphics.FillPath(brush, rect)

                    graphics.SmoothingMode <- SmoothingMode.Default

            member this.ViewMoved leftMostRecord = 
                if not displayed then
                    fader.Post "fadeIn"

                checkMovement.Post "viewmoved"

            member this.MouseDown () = isMouseDown <- true

            member this.MouseUp () = 
                isMouseDown <- false
                if not isMoving then
                    fader.Post "fadeOut"

            [<CLIEvent>]
            member this.RedrawEvent = redrawEvent.Publish