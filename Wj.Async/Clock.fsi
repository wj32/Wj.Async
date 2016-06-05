namespace Wj.Async

open System

module Clock =
  val after : TimeSpan -> unit IDeferred
  val afterMs : int -> unit IDeferred
  val at : DateTime -> unit IDeferred
