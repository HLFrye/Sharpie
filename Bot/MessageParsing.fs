module MessageParsing

open System
open Types

let (|NumericErrorInput|_|) input =
    let succeeded, errorCode = Int32.TryParse(input)
    if succeeded 
    then Some errorCode
    else None

type MessageTokenizer(msg: string) =
    let mutable currentIdx = 0

    member this.peek () = msg.Substring(currentIdx, (msg.IndexOf(' ', currentIdx) - currentIdx))
    member this.pop () = 
        let result = this.peek ()
        currentIdx <- msg.IndexOf(' ', currentIdx) + 1
        result

    member this.getRemainingText () = msg.Substring(currentIdx + 1)

let parseMsg (msg: string) = 
    let tokenizer = new MessageTokenizer(msg)

    let sender = 
        match tokenizer.peek().[0] with
        | ':' -> 
            Some (tokenizer.pop())
        | _ -> None
       
    let msgType = 
        match tokenizer.pop() with
        | "NOTICE" -> 
            Notice (tokenizer.pop(), tokenizer.getRemainingText())
        | "ERROR" -> Error (tokenizer.getRemainingText())
        | "PING" -> Ping (tokenizer.getRemainingText())
        | NumericErrorInput code -> 
            let sender =
                match tokenizer.peek().[0] with
                | ':' -> None                    
                | _ -> Some (tokenizer.pop()) 
            NumericError (code, sender, tokenizer.getRemainingText())
        | _ -> Unparseable msg
    {
        Sender = sender;
        MsgType = msgType
    }        
