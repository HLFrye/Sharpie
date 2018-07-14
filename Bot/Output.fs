module Output


open Types

let sprintNotice sender target =
    match sender with
    | Some x -> sprintf "[NOTICE]: %s -> %s: %s" x target
    | None -> sprintf "[NOTICE]: -> %s: %s" target

let sprintError = sprintf "[ERROR]: %s"

let sprintNumericError sender code target =
    match sender, target with
    | None, None -> sprintf "[ERROR %d]: %s" code
    | Some x, None -> sprintf "[ERROR %d]: <%s> %s" code x 
    | Some x, Some y -> sprintf "[ERROR %d]: %s -> %s: %s" code x y 
    | None, Some x -> sprintf "[ERROR %d]: -> %s: %s" code x

let sprintMessage (msg: ParsedMsg) =
    match msg.MsgType with
    | Notice (x, y) -> sprintNotice msg.Sender x y
    | Error x -> sprintError x
    | NumericError (code, x, y) -> sprintNumericError msg.Sender code x y
    | Unparseable x -> sprintf "Failed to parse %s" x
    | Ping x -> sprintf "Ping '%s'" x

let printMessage = (sprintMessage >> printfn "%s")