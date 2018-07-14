module Types

type MsgType = 
| Notice of string * string
| Error of string
| NumericError of int * string option * string
| Ping of string
| Unparseable of string

type ParsedMsg = {
    Sender: string option
    MsgType: MsgType
}