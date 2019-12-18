module Domain

type State = int

type Message =
    | Increment
    | Decrement

let init () : State =
    0

let update (msg : Message) (model : State) : State =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1
