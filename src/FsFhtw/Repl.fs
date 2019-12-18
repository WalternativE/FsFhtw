module Repl

open System

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Increment|Decrement|Help|ParseFailed|) (input : string) =
    match input with
    | i when safeEquals i (nameof Domain.Increment) -> Increment
    | i when safeEquals i (nameof Domain.Decrement) -> Decrement
    | i when safeEquals i HelpLabel -> Help
    | _ -> ParseFailed

let read (input : string) =
    match input with
    | Increment -> Domain.Increment |> DomainMessage
    | Decrement -> Domain.Decrement |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update : Domain.Message -> State -> State) (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let message = sprintf "The message was %A. New state is %A" msg newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : State) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop

    loop state
