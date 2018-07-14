open System
open System.Net
open System.Net.Sockets

open Types

#nowarn "40"


type Message = 
| Received of line: string
| Closed of remainingBuffer: string    

let parsedMessageProcessor stream =
    let mutable state = ClientState.ConnectionStarted
    MailboxProcessor.Start(fun inbox -> 
        let rec loop =
            async {
                let! nextState = ClientState.processState stream state inbox
                printfn "Transitioning from %A to %A" state nextState
                state <- nextState 
                return! loop
            }
        loop)

let messageProcessor (outbox: MailboxProcessor<ParsedMsg>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop =
            async {                     
                    let! msg = inbox.Receive() 
                    match msg with
                    | Received x -> 
                        printfn "Received: %s" x
                        let parsed = x |> MessageParsing.parseMsg
                        parsed |> (outbox.Post)
                        return! loop
                    | Closed x ->
                        do printf "Connection closed (%s)" x
                        return ()                     
                    }
        loop)

let rec asyncReadMessage (processor: MailboxProcessor<Message>) buffer (stream: NetworkStream) =
    async {
        let nextByte = stream.ReadByte()
        if nextByte = -1 
        then 
            processor.Post(buffer |> Closed)
            return 0
        else        
            let nextChar = nextByte |> Char.ConvertFromUtf32
            let newBuffer = buffer + nextChar
            if newBuffer.EndsWith("\n")
            then
                processor.Post(newBuffer.TrimEnd() |> Received)
                return! (asyncReadMessage processor "" stream)
            else            
                return! (asyncReadMessage processor newBuffer stream)
    }

[<EntryPoint>]
let main argv =
    printfn "Connecting to %s:%s" argv.[0] argv.[1]
    let server = argv.[0]
    let port = 
        match argv.[1] |> Int32.TryParse with
        | false, _ -> sprintf "Invalid port input: %s" argv.[1] |> failwith
        | true, x when x <= 0 || x > 65535 -> sprintf "Port out of range: %s" argv.[1] |> failwith
        | true, x -> x
    let ip = Dns.GetHostAddresses(server) 
    printfn "Resolved to %s:%d" (ip.[0].ToString()) port
    let client = new TcpClient();
    client.Connect(ip, port) |> ignore
    let stream = client.GetStream()
    let parsedMsgProcessor = parsedMessageProcessor stream
    asyncReadMessage (messageProcessor parsedMsgProcessor) "" stream |> Async.RunSynchronously |> ignore
    0 // return an integer exit code
