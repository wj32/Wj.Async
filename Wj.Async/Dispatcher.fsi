namespace Wj.Async

module Dispatcher =
  // IDispatcher functions
  val enqueue : IDispatcher -> supervisor : ISupervisor -> f : (unit -> unit) -> unit
  val run : IDispatcher -> f : (unit -> 'a IDeferred) -> 'a

  val current : unit -> IDispatcher
  val create : unit -> IDispatcher
