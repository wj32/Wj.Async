namespace Wj.Async

open System

module Clock =
  val after : span : TimeSpan -> unit IDeferred
  val afterMs : ms : int -> unit IDeferred
  val at : time : DateTime -> unit IDeferred
