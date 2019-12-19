module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Increment|Decrement|IncrementBy|DecrementBy|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb ] when safeEquals verb (nameof Domain.Increment) -> Increment
    | [ verb ] when safeEquals verb (nameof Domain.Decrement) -> Decrement
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb; arg ] when safeEquals verb (nameof Domain.IncrementBy) ->
        tryParseInt arg (fun value -> IncrementBy value)
    | [ verb; arg ] when safeEquals verb (nameof Domain.DecrementBy) ->
        tryParseInt arg (fun value -> DecrementBy value)
    | _ -> ParseFailed
