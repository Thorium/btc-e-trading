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

module GraphFunctions =

    open System
    open System.Drawing
    open System.Windows.Forms
    open System.Drawing.Drawing2D

    let abs (x: float) = System.Math.Abs(x)

    let absInt (x: int) = System.Math.Abs(x)
 
    let log10 (x: float) = System.Math.Log10(x)
 
    let floor (x: float) = System.Math.Floor(x)
 
    let ceiling (x: float) = System.Math.Ceiling(x)

    type Direction =
    | Left
    | Right
 
    /// <summary>
    /// Get a list of values between given high and low values rounded to the nearest round to value.
    /// The lowest value will be the lowest rounded value above low, and the highest value the 
    /// highest rounded value below high.
    /// </summary>
    /// <param name="high">Highest value. Must be greater than or equal to low.</param>
    /// <param name="low">Lowest value. Must be less than or equal to high.</param>
    /// <param name="roundTo">The values returned will be rounded to the nearest number * (10 ^ x) in this list.</param>
    /// <param name="roughNumberOfValues">Roughly the number of values to be returned, as the numbers returned are rounded
    /// they may not exactly fit the number of values given.</param>
    /// <returns>List of values between given high and low values rounded to the nearest power of ten 
    /// of the nearest round to (roundTo) value.</returns>
    let getRoundedValuesBetween (high: float) low (roundTo: uint16 list) roughNumberOfValues =
        if high < low then raise(new System.ArgumentException("high must be greater than or equal to low.", "high"))
        if roundTo.IsEmpty then raise(new System.ArgumentException("roundTo must not be empty.", "roundTo"))
        if roughNumberOfValues <= 0 then raise(new System.ArgumentException("roughNumberOfValues must greater than 0.", "roughNumberOfValues"))
 
        if high = low then
            [high], high, low
        else
            let heightOfValue = abs(high - low) / float(roughNumberOfValues)
 
            let ceilingExponent = ceiling(log10(heightOfValue))
            let floorExponent = floor(log10(heightOfValue))
 
            let minRoundTo = float(List.min roundTo) * (10.0 ** ceilingExponent)
 
            let roundTo = minRoundTo :: List.map (fun x -> float(x) * (10.0 ** floorExponent)) roundTo
 
            let closestToRoundTo = List.minBy (fun x -> abs(heightOfValue - x)) roundTo
 
            let lowestLabel = closestToRoundTo * floor(low / closestToRoundTo)
            let highestLabel = closestToRoundTo * ceiling(high / closestToRoundTo)
 
            let numberOfLabels = abs(highestLabel - lowestLabel) / closestToRoundTo

            let labels = [ for i in 0..int(numberOfLabels) do yield lowestLabel + closestToRoundTo * float(i) ]
 
            labels, highestLabel, lowestLabel
 
    let foldRecordsToHighLow folder records =
        let startingValues = System.Double.MinValue, System.Double.MaxValue
 
        Array.fold folder startingValues records

    let getNumberOfRecordsCanBeDisplayed widthOfView candleWidth candleLeftMargin =
        widthOfView / (candleWidth + candleLeftMargin)

    let mapXCoordinateToRecordNumber x candleWidth candleLeftMargin leftMostRecord =
        floor(x / float(candleWidth + candleLeftMargin)) + float(leftMostRecord)

    let mapRecordNumberToXCoordinate recordNumber candleWidth candleLeftMargin leftMostRecord =
        (recordNumber - leftMostRecord) * (candleWidth + candleLeftMargin)

    let mapValueToYCoordinate heightOfView (highestHigh, lowestLow) gap value =
        let pixel = float32(heightOfView) / (highestHigh - lowestLow + gap * float32(2))
        (highestHigh + gap - value) * pixel

    let mapHeight heightOfView height (highestHigh, lowestLow) gap =
        let pixel = float32(heightOfView) / (highestHigh - lowestLow + float32(gap) * float32(2))
        height * pixel

    let createRoundedRectangle x y width height radius =
        let xw = x + width
        let yh = y + height
        let xwr = xw - radius
        let yhr = yh - radius
        let xr = x + radius
        let yr = y + radius
        let r2 = radius * 2
        let xwr2 = xw - r2
        let yhr2 = yh - r2

        let path = new GraphicsPath()
        path.StartFigure()

        // Top Left Corner
        path.AddArc(x, y, r2, r2, 180.0f, 90.0f)

        // Top Edge
        path.AddLine(xr, y, xwr, y)

        // Top Right Corner
        path.AddArc(xwr2, y, r2, r2, 270.0f, 90.0f)

        // Right Edge
        path.AddLine(xw, yr, xw, yhr);

        // Bottom Right Corner
        path.AddArc(xwr2, yhr2, r2, r2, 0.0f, 90.0f)

        // Bottom Edge
        path.AddLine(xwr, yh, xr, yh)

        // Bottom Left Corner
        path.AddArc(x, yhr2, r2, r2, 90.0f, 90.0f)

        // Left Edge
        path.AddLine(x, yhr, x, yr)

        path.CloseFigure()
        path

    let getCandleStickLine limits (high, low) x gap candleWidth heightOfView =
        let x = x + float32(ceiling(float(candleWidth / 2)))

        let mapValueToY = mapValueToYCoordinate heightOfView limits gap

        PointF(x, mapValueToY high), PointF(x, mapValueToY low)
 
    let getCandleStick limits (high, low, opening, closing) x gap candleWidth heightOfView =
        let top, bottom = if opening > closing then opening, closing else closing, opening

        let y = mapValueToYCoordinate heightOfView limits gap top

        let height = mapHeight heightOfView (top - bottom) limits (int gap)

        RectangleF(x, y, float32(candleWidth), height)

    let paintCandleStick (graphics:Graphics) (high: float, low: float, opening: float, closing: float) candlestickNumber limits gap candleWidth candleLeftMargin leftMostRecord heightOfView =
        let candleColour = if closing > opening then Color.Green else Color.Red

        use pen = new Pen(candleColour, float32(0.5))
        use brush = new SolidBrush(candleColour)

        let x = mapRecordNumberToXCoordinate candlestickNumber candleWidth candleLeftMargin leftMostRecord |> float32

        let rect = getCandleStick limits (float32(high), float32(low), float32(opening), float32(closing)) x gap candleWidth heightOfView
 
        graphics.FillRectangle(brush, rect)
        
        let high, low = getCandleStickLine limits (float32(high), float32(low)) x gap candleWidth heightOfView

        graphics.DrawLine(pen, high, low)
 
    let paintCandleSticks (graphics:Graphics) (high, low) gap records candleWidth candleLeftMargin leftMostRecord heightOfView =
        let paintCandleStick = paintCandleStick graphics
        Array.iteri (fun i record -> paintCandleStick record (i + leftMostRecord) (high, low) gap candleWidth candleLeftMargin leftMostRecord heightOfView) records
 
    let paintCoordinates (graphics:Graphics) (lastMouseX, lastMouseY) (widthOfView, heightOfView) =
        let startHorizontal, endHorizontal = Point(0, lastMouseY), Point(widthOfView, lastMouseY)
        let startVertical, endVertical = Point(lastMouseX, 0), Point(lastMouseX, heightOfView)
 
        use pen = new Pen(Color.DarkGray, float32(0.5))
 
        graphics.DrawLine(pen, startHorizontal, endHorizontal)
        graphics.DrawLine(pen, startVertical, endVertical)
 
    let paintYAxisLabel (graphics:Graphics) y label font widestLabelWidth widthOfView =
        let rightMargin = 5
 
        let stringMeasurements = graphics.MeasureString(label, font)
        let stringLength = stringMeasurements.Width
 
        use pen = new Pen(Color.FromArgb(170, 170, 170), float32(0.5))
 
        let lineLength = float32(10)
 
        let labelPadding = float32(25)

        let textY = y - float32(stringMeasurements.Height) / float32(2)
 
        let lineStart = PointF(float32(widthOfView), y)
        let lineEnd = PointF(lineStart.X - lineLength, y)
        graphics.DrawLine(pen, lineStart, lineEnd)
 
        let textStart = PointF(lineEnd.X - widestLabelWidth - labelPadding, textY)
        graphics.DrawString(label, font, new SolidBrush(Color.FromArgb(170, 170, 170)), textStart)
 
        let lineStart = PointF(textStart.X - labelPadding, y)
        let lineEnd = PointF(lineStart.X - lineLength, y)
        graphics.DrawLine(pen, lineStart, lineEnd)
 
    let paintYAxis (graphics:Graphics) (labels: 'a list) (widthOfView, heightOfView) =
        use font = new Font("Consolas", float32(8))

        let labels = List.map (fun label -> label.ToString()) labels

        let widestLabelWidth = List.fold (fun width label -> 
            let currentWidth = graphics.MeasureString(label, font).Width
            if currentWidth > width then currentWidth else width) (float32(0)) labels
 
        let paintLabel i label =
            let y = (float32(heightOfView) / float32(labels.Length + 1)) * float32(labels.Length - i)
            paintYAxisLabel graphics y label font widestLabelWidth widthOfView
 
        List.iteri (fun i x -> paintLabel i <| x.ToString()) labels