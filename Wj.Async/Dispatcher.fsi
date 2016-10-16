namespace Wj.Async

module Dispatcher =
  // IDispatcher functions
  val inline enqueue : IDispatcher -> supervisedCallback : unit SupervisedCallback -> unit
  val inline run : IDispatcher -> f : (unit -> 'a IDeferred) -> 'a
  val inline rootSupervisor : IDispatcher -> ISupervisor
  val inline registerScheduler : IDispatcher -> scheduler : IScheduler -> ISchedulerRegistration

  val current : unit -> IDispatcher
  val create : unit -> IDispatcher

  val enqueueRoot : IDispatcher -> f : (unit -> unit) -> unit
