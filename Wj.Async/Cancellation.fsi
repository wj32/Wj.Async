namespace Wj.Async

open System.Threading

module Cancellation =
  type T = unit IDeferred

  module Source =
    type T = unit IVar

    val inline create : unit -> T
    val inline set : T -> unit


  val now : T
  val never : T

  val inline isSet : T -> bool
  val raiseIfSet : T -> unit
  val run : T -> f : (unit -> 'a IDeferred) -> 'a IDeferred

  val inline ofSource : Source.T -> T
  val ofToken : CancellationToken -> T
  val toToken : T -> CancellationToken
