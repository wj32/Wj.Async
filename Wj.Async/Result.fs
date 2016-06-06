namespace Wj.Async

open System

module Result =
  type T<'a, 'b> =
    | Success of 'a
    | Failure of 'b

  let ofSuccess x = Success x

  let ofFailure y = Failure y

  let tryWith f =
    try
      Success (f ())
    with ex ->
      Failure ex
