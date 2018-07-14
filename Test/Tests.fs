module Tests

open System
open Xunit

open Types
open MessageParsing
open Output

[<Fact>]
let ``Parse Ping Message`` () =
    let testMsg = "PING :snespi.local"
    let parsed = testMsg |> parseMsg
    let expected = {
        Sender = None
        MsgType = Ping "snespi.local" 
    } 
    Assert.Equal((expected |> sprintMessage), (parsed |> sprintMessage))

[<Fact>]
let ``Parse 372 Message`` () =
    let testMsg = ":snespi.local 372 Sharpie :-      ,g$$$$$$$$$$$$$$$P.    ---------------"
    let parsed = testMsg |> parseMsg
    let expected = {
        Sender = Some ":snespi.local"
        MsgType = NumericError (372, (Some "Sharpie"), "-      ,g$$$$$$$$$$$$$$$P.    ---------------") 
    } 
    Assert.Equal((expected |> sprintMessage), (parsed |> sprintMessage))
