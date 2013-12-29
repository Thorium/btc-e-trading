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

module KineticScroller =

    open System
    open System.Windows.Forms

    open IMovementListener
    
    type KineticScroller() =

        let cancelScrolling, move = Event<unit>(), Event<int>()
        
        let mouseMovements = System.Collections.Generic.Queue<int * int64>()
        let maxVelocity, friction = 30m, 1m

        [<CLIEvent>]
        member this.CancelScrolling = cancelScrolling.Publish

        interface IMovementListener with 

            [<CLIEvent>]
            member this.Move = move.Publish

            member this.OnMouseMove (event:MouseEventArgs) =
                if event.Button = MouseButtons.Left then
                    let maxMouseMovementsRecorded = 5
                    if mouseMovements.Count > maxMouseMovementsRecorded then
                        mouseMovements.Dequeue() |> ignore
                    mouseMovements.Enqueue(event.X, System.DateTime.Now.Ticks)

            member this.OnMouseDown (event:MouseEventArgs) =
                mouseMovements.Clear()
                mouseMovements.Enqueue(event.X, DateTime.Now.Ticks)
                cancelScrolling.Trigger()

            member this.OnMouseUp (event:MouseEventArgs) =
                if mouseMovements.Count > 1 then
                    let velocity (mouseMovements: System.Collections.Generic.Queue<int * int64>) =
                        let firstX, firstTime = mouseMovements.Peek()

                        let rec getMouseMovedStats distance time =
                            if mouseMovements.Count > 0 then
                                let x, time = mouseMovements.Dequeue()
                                let milliseconds = time / TimeSpan.TicksPerMillisecond - firstTime / TimeSpan.TicksPerMillisecond
                                getMouseMovedStats (x + (firstX - distance)) milliseconds
                            else
                                distance, time

                        let distance, time = getMouseMovedStats 0 <| int64 0

                        let distance, time = float distance, float time

                        let velocity = decimal <| (abs(distance) / time) * (if distance < 0.0 then -5.0 else 5.0)

                        if velocity > maxVelocity then maxVelocity
                        else if velocity < -maxVelocity then -maxVelocity
                        else velocity
            
                    let kineticScroll = new MailboxProcessor<string>(fun inbox ->
                        let rec loop i timeout (velocity: decimal) =
                            async { 
                                let! result = inbox.TryReceive timeout

                                if result.IsSome then
                                    mouseMovements.Clear()
                                else
                                    let velocity = 
                                        if Math.Floor(velocity) > 0m then 
                                            velocity - friction
                                        else if Math.Ceiling(velocity) < 0m then
                                            velocity + friction
                                        else
                                            0m

                                    move.Trigger (int <| ceil velocity)

                                    if not <| (Math.Abs(velocity) < Math.Abs(friction)) then
                                        return! loop (i + 1) timeout velocity
                            } 
                        loop 0 25 (velocity mouseMovements))

                    kineticScroll.Start()

                    Event.add (fun _ -> kineticScroll.Post "stop") cancelScrolling.Publish