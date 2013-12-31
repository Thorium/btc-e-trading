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
    open System.Threading

    /// Scrollbar that shows movement, fades in on the start of movement and fades out at the end of movement.
    /// The scrollbar can only be viewed by the user kind of "read-only".
    type Scrollbar() =
        let redrawEvent = Event<unit>()

        let mutable fadingIn, fadingOut, displayed = None, None, false

        let mutable isMoving, isMouseDown = false, false

        let rec fade alpha func until pause apply =
            async {
                if until alpha then
                    apply alpha
                    do! Async.Sleep(pause)
                    fade (func alpha) func until pause apply
            } |> Async.StartImmediate

        let fadeOut () =
            let until x =
                let isFinished = x <= 0
                if isFinished then
                    fadingOut <- None
                    displayed <- false
                not isFinished
            async { fade 255 (fun x -> redrawEvent.Trigger(); x - 40) until 25 (fun x -> fadingOut <- Some(x)) } |> Async.StartImmediate

        let fadeIn () = 
            displayed <- true
            let until x =
                let isFinished = x > 255
                if isFinished then
                    fadingIn <- None
                not isFinished
            async { fade 0 (fun x -> redrawEvent.Trigger(); x + 40) until 25 (fun x -> fadingIn <- Some(x)) } |> Async.StartImmediate

        let scrollerColor () =
            match fadingIn, fadingOut with
            | Some(alpha), None -> Color.FromArgb(alpha, Color.DarkGray)
            | None, Some(alpha) -> Color.FromArgb(alpha, Color.DarkGray)
            | None, None -> Color.DarkGray
            | _ -> failwith "Attempting to fade in and fade out at the same time."

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
                        fadeOut()
                        isMoving <- false
                    | _ -> ()
                        
                    do! loop timeout
                } 
            loop 75)

        do
            checkMovement.Start()
            
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
                    use brush = new SolidBrush(scrollerColor())
                    graphics.FillPath(brush, rect)

                    graphics.SmoothingMode <- SmoothingMode.Default

            member this.ViewMoved leftMostRecord = 
                if not displayed then
                    fadeIn()

                checkMovement.Post "viewmoved"

            member this.MouseDown () = isMouseDown <- true

            member this.MouseUp () = 
                isMouseDown <- false
                if not isMoving then
                    fadeOut()

            [<CLIEvent>]
            member this.RedrawEvent = redrawEvent.Publish