module TaLibTests

open NUnit.Framework

[<TestFixture>]
type TestTaLib() = class
    [<Test>]
    member self.cdlShootingStar() = 
        let opening = [| for i in 1..100 do yield float(i) |]
        let closing = [| for i in 1..100 do yield float(i) |]
        let high = [| for i in 21..120 do yield float(i) |]
        let low = [| for i in 11..110 do yield float(i) |]

        let data = TaLib.Library.PatternRecognition.cdl2Crows 0 99 opening closing high low

        ignore
end