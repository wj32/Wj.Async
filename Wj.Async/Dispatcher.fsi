namespace Wj.Async

module Dispatcher =
  val current : unit -> IDispatcher
  val tryCurrent : unit -> IDispatcher option
  val create : unit -> IDispatcher

  // Interface functions
  val enqueue : IDispatcher -> (unit -> unit) -> unit
  val run : IDispatcher -> (unit -> 'a IDeferred) -> 'a
