namespace Wj.Async

open System

module Result =
  type T<'a, 'b> =
    | Success of 'a
    | Failure of 'b

  val ofSuccess : 'a -> T<'a, 'b>
  val ofFailure : 'b -> T<'a, 'b>

  val tryWith : (unit -> 'a) -> T<'a, exn>
