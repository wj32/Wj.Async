namespace Wj.Async

module Dispatcher =
  // IDispatcher functions
  val enqueue : IDispatcher -> (unit -> unit) -> unit
  val run : IDispatcher -> (unit -> 'a IDeferred) -> 'a

  val current : unit -> IDispatcher
  val tryCurrent : unit -> IDispatcher option
  val create : unit -> IDispatcher
