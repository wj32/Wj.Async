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
  val inline raiseIfSet : T -> unit
  val run : f : (unit -> 'a IDeferred) -> 'a option IDeferred

  val inline ofSource : Source.T -> T
  val ofToken : CancellationToken -> T
  val toToken : T -> CancellationToken

  module Option =
    val inline join : T option -> T
    val inline isSet : T option -> bool
    val inline raiseIfSet : T option -> unit
    val inline ofSource : Source.T -> T option
    val inline ofToken : CancellationToken -> T option
    val inline toToken : T option -> CancellationToken
