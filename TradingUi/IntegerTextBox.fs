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

namespace TradingUi

module IntegerTextBox =

    open System.Windows
    open System.Windows.Input
    open System.Windows.Controls

    /// Text box that only lets the user enter integer up to a given max value.
    type IntegerTextBox(maxValue) =
        inherit TextBox()

        let integerChanged = new Event<_>()

        new(maxValue) as this = IntegerTextBox(maxValue) then
            DataObject.AddPastingHandler(this, DataObjectPastingEventHandler(fun _ _ -> ()))

        [<CLIEvent>]
        member this.IntegerChanged = integerChanged.Publish

        member public this.GetInteger() =
            int(this.Text)

        override this.OnPreviewKeyDown(e) =
            let key = e.Key
            let isNotNumber = key <> Key.Back && key <> Key.Delete && (key < Key.NumPad0 || key > Key.NumPad9) && (key < Key.D0 || key > Key.D9)
            let isNotArrowKey = key <> Key.Left && key <> Key.Right

            e.Handled <- isNotNumber && isNotArrowKey

        override this.OnPreviewTextInput(e) =
            e.Handled <- int(this.Text.Insert(this.SelectionStart, e.Text)) > maxValue

        override this.OnTextChanged(e) =
            if this.Text.Length > 0 then
                integerChanged.Trigger(this, Some(int(this.Text)))
            else
                integerChanged.Trigger(this, None)