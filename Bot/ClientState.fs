module ClientState

open System
open System.Net
open System.Net.Sockets
open Types
open Output
open System.Text

type ClientState =
| ConnectionStarted
| PreambleStarted
| PreambleFinished
| NickSent of string
| NickAccepted 
| Ready

type ClientCommand =
| NickCommand of string
| UserCommand of username: string * realname: string
| PongResponse of string

let FormatMsg = function
| NickCommand nick -> sprintf "NICK %s" nick
| UserCommand (username, realname) -> sprintf "USER %s 0 * :%s" username realname
| PongResponse msg -> sprintf "PONG :%s" msg

let toBytes (msgText: string) = 
    let bytes = Encoding.ASCII.GetBytes msgText
    (bytes, 0, (bytes |> Array.length))

let SendCommandToNetwork (stream: NetworkStream) msg =
    async {
        let outputMsg = msg |> FormatMsg
        printfn "Sending: %A" outputMsg
        let output = outputMsg |> (sprintf "%s\n") |> toBytes
        let (buf, offset, count) = output
        do! stream.WriteAsync(buf, offset, count) |> Async.AwaitTask
    }

open System.Text.RegularExpressions

let getNextNick lastNick =
    let regexMatch = Regex.Match(@"(?<base>^\d*)(?<number>\d*)", lastNick)
    match regexMatch with
    | null -> "Sharpie"
    | x -> 
        let baseName = x.Groups.["base"].Value
        let nextNum = 
            x.Groups.["number"].Value 
            |> Int32.TryParse 
            |> (fun (x, y) -> if x then y else 0)
            |> (+) 1
        sprintf "%s%d" baseName nextNum        

let handleConnectionStarted (inbox: MailboxProcessor<ParsedMsg>): Async<ClientState> =
    async {
        let! msg = inbox.Receive()
        return PreambleStarted
    }

let handlePreambleStarted (inbox: MailboxProcessor<ParsedMsg>): Async<ClientState> =
    async {
        let! msgOption = inbox.TryReceive(200) 
        let msgReceived = msgOption |> Option.isSome
        return if msgReceived then PreambleStarted else PreambleFinished
    }

let handlePreambleFinished (stream: NetworkStream) (inbox: MailboxProcessor<ParsedMsg>) = 
    async {
        let initialName = "Sharpie"
        do! initialName |> NickCommand |> SendCommandToNetwork stream
        return initialName |> NickSent
    }

let handleNickSent (stream: NetworkStream) nick (inbox: MailboxProcessor<ParsedMsg>) =
    async {
        let! msgOption = inbox.TryReceive(500)
        match msgOption with
        | None -> return NickAccepted
        | Some x -> 
            match x.MsgType with 
            | NumericError (433, (Some nick), msg) -> 
                let newNick = nick |> getNextNick
                do! newNick |> NickCommand |> SendCommandToNetwork stream
                return newNick |> NickSent
            | _ -> return nick |> NickSent
    }

let handleNickAccepted (stream: NetworkStream) _ =
    async {
        do! ("Sharpie", "An F# Bot!") |> UserCommand |> SendCommandToNetwork stream
        return Ready
    }

let readyProcess (stream: NetworkStream) (inbox: MailboxProcessor<ParsedMsg>) =
    async {
        let! msg = inbox.Receive()
        do Output.printMessage msg
        match msg.MsgType with
        | Ping msg -> 
            do! msg |> PongResponse |> SendCommandToNetwork stream
        | _ -> msg |> Output.printMessage

        return Ready 
    }

let processState stream = function
| ConnectionStarted -> handleConnectionStarted
| PreambleStarted -> handlePreambleStarted
| PreambleFinished -> handlePreambleFinished stream
| NickSent nick -> handleNickSent stream nick
| NickAccepted -> handleNickAccepted stream
| Ready -> readyProcess stream
