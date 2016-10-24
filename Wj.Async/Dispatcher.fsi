namespace Wj.Async

module Dispatcher =
  // IDispatcher functions
  val inline enqueue : IDispatcher -> supervisedCallback : unit SupervisedCallback -> unit
  val inline run : IDispatcher -> f : (unit -> 'a IDeferred) -> 'a
  val inline rootSupervisor : IDispatcher -> ISupervisor

  val current : unit -> IDispatcher
  val create : unit -> IDispatcher

  val enqueue' : IDispatcher -> supervisor : ISupervisor -> f : (unit -> 'a IDeferred) -> 'a IDeferred
  val enqueueRoot : IDispatcher -> f : (unit -> unit) -> unit
