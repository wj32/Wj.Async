namespace Wj.Async

module Dispatcher =
  // IDispatcher functions
  val enqueue : IDispatcher -> supervisedCallback : unit SupervisedCallback -> unit
  val run : IDispatcher -> f : (unit -> 'a IDeferred) -> 'a
  val rootSupervisor : IDispatcher -> ISupervisor

  val current : unit -> IDispatcher
  val create : unit -> IDispatcher
