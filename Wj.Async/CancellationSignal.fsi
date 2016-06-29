namespace Wj.Async

open System.Threading

module CancellationSignal =
  type T = unit IDeferred
  type Source = unit IVar

  val now : T
  val never : T

  val inline isSet : T -> bool
  val raiseIfSet : T -> unit

  val ofToken : CancellationToken -> T
  val toToken : T -> CancellationToken

  val inline create : unit -> Source
  val inline ofSource : Source -> T
  val inline set : Source -> unit
