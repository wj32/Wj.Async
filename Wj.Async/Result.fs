namespace Wj.Async

open System

module Result =
  type T<'a, 'error> =
    | Success of 'a
    | Failure of 'error

  let mapError t f = match t with Success x -> Success x | Failure error -> Failure (f error)

  let ofSuccess x = Success x

  let ofFailure error = Failure error

  let ofChoice choice =
    match choice with Choice1Of2 x -> Success x | Choice2Of2 error -> Failure error

  let tryWith f =
    try
      Success (f ())
    with ex ->
      Failure ex

  let ``return`` x = ofSuccess x

  let bind t f = match t with Success x -> f x | Failure error -> Failure error

  let map t f = match t with Success x -> Success (f x) | Failure error -> Failure error

  let join t = bind t id

  let forget t = map t ignore

  let all ts =
    let rec loop acc ts =
      match ts with
      | [] -> Success (List.rev acc)
      | t :: ts ->
        match t with
        | Success x -> loop (x :: acc) ts
        | Failure error -> Failure error
    loop [] ts

  let allForget ts =
    let rec loop ts =
      match ts with
      | [] -> Success ()
      | t :: ts ->
        match t with
        | Success () -> loop ts
        | Failure error -> Failure error
    loop ts

  module Infix =
    let (>>=) t f = bind t f
    let (>>|) t f = map t f
