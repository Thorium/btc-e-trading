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

namespace BtceApiFramework

module HttpRequestHandler = 

    open System.Net
    open System
    open System.IO
    open System.Security.Cryptography
    open System.Text

    let getNonce() : string =
        let unixTime = (DateTime.UtcNow - DateTime(1970, 1, 1, 0, 0, 0))
        ((int64)unixTime.TotalSeconds).ToString()

    let buildQueryString (parameters: (string * string) list) (generateNonce: unit -> string) : string =
        let getFieldValue (s: string * string) : string = 
            let field, value = s
            field + "=" + value

        let m = List.map getFieldValue parameters
        System.String.Join("&", m) + "&" + getFieldValue("nonce", generateNonce())

    let httpRequestHandler (url: string) (key: string) (secret: string) (parameters: (string * string) list) : string =
        let dataStr = buildQueryString parameters getNonce
        let data = Encoding.ASCII.GetBytes(dataStr)

        let request = WebRequest.Create(url)

        request.Method          <- "POST"
        request.ContentType     <- "application/x-www-form-urlencoded";
        request.ContentLength   <- (int64)dataStr.Length;
        request.Headers.Add("Key", key)

        let encoding = new Text.ASCIIEncoding()
        let keyByte = encoding.GetBytes(secret)
        let hmacsha512 = new HMACSHA512(keyByte)
        let messageBytes = encoding.GetBytes(dataStr)
        let hashmessage = hmacsha512.ComputeHash(messageBytes)

        let s2 = BitConverter.ToString(hashmessage).Replace("-", "").ToLower()

        request.Headers.Add("Sign", s2)

        let reqStream = request.GetRequestStream()
        reqStream.Write(data, 0, data.Length)
        reqStream.Close()

        let response = request.GetResponse()
        let resStream = response.GetResponseStream()
        let resStreamReader = new StreamReader(resStream)

        resStreamReader.ReadToEnd()