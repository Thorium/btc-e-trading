#BTC-E Framework

###About

A complete F# interface for btc-e's api, https://btc-e.com/api/documentation and https://btc-e.com/api/3/documentation. Supports all public v3 and private api calls.

Example of getting the balance of your account:

```
open BtceApiFramework

let key = "yourkey"
let secret = "yoursecret"

let response = PrivateBtceApi.getAccountInformation key secret

match response with
    | Response.Success(accountInfo) -> 
        let funds = accountInfo.funds
        System.Console.Write("BTC: " + funds.btc.ToString() + " USD: " + funds.usd.ToString())
    | Response.Error(error) -> System.Console.Write("Failed with error, " + error)
```

###Dependencies

* JSON.net is used for deserializing JSON responses, http://james.newtonking.com/pages/json-net.aspx

The project is licensed under GPLv3. For more information on the license see the LICENSE file and http://www.gnu.org/licenses/quick-guide-gplv3.html.