module FsFhtw.Tests

open Xunit
open FsCheck

[<Fact>]
let ``That the laws of reality still apply`` () =
    Assert.True(1 = 1)

[<Fact>]
let ``That incrementing twice on an initialized counter yields 2`` () =
    let initialState = Domain.init ()

    let actual =
        Domain.update Domain.Increment initialState
        |> Domain.update Domain.Increment

    let expected = 2

    Assert.Equal(expected, actual)

// not the best helper function for property based tests
// because itselve has testable behavior
let getInverse (message : Domain.Message) =
    match message with
    | Domain.Increment -> Domain.Decrement
    | Domain.Decrement -> Domain.Increment
    | Domain.IncrementBy x -> Domain.DecrementBy x
    | Domain.DecrementBy x -> Domain.IncrementBy x

[<Fact>]
let ``That applying the inverse of counter event yields the initial state`` () =
    let prop (message : Domain.Message) =
        let initialState = Domain.init ()

        let actual =
            initialState
            |> Domain.update message
            |> Domain.update (getInverse message)

        actual = initialState

    Check.QuickThrowOnFailure prop
