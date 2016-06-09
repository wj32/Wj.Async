namespace Wj.Async

module DeferredSeq =
  open Deferred.Infix

  type 'a T = 'a Next IDeferred
  and 'a Next =
    | Empty
    | Cons of head : 'a * tail : 'a T

  module Writer =
    let [<Literal>] private WriterAlreadyClosed = "The writer is already closed."

    type 'a State =
      | Closed
      | Tail of 'a Next IVar

    type 'a T = {mutable state : 'a State}

    let createTail () =
      let tail = Deferred.createVar ()
      (tail :> _ IDeferred, {state = Tail tail})

    let isClosed t =
      match t.state with
      | Closed -> true
      | Tail _ -> false

    let close t =
      match t.state with
      | Closed -> invalidOp WriterAlreadyClosed
      | Tail tail ->
        Empty --> tail
        t.state <- Closed

    let write t value =
      match t.state with
      | Closed -> invalidOp WriterAlreadyClosed
      | Tail tail ->
        let tail' = Deferred.createVar ()
        Cons (value, tail') --> tail
        t.state <- Tail tail'

  let create () = Writer.createTail ()

  let empty () = Deferred.value Empty
