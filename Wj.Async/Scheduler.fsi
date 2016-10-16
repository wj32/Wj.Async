namespace Wj.Async

open System.Collections.Generic

module Scheduler =
  // IScheduler functions
  val inline dispatchers : IScheduler -> IDispatcher ICollection
  val inline acceptingEnqueue : IScheduler -> bool
  val inline enqueue : IScheduler -> schedulerCallback : SchedulerCallback -> unit
  val inline take : IScheduler -> ISchedulerTake option
  val inline queueDepth : ISchedulerTake -> int
  val inline registerEnqueuedCallback : ISchedulerTake -> callback : (unit -> unit) -> IRegistration
  val inline dequeue : ISchedulerTake -> unit SupervisedCallback

  val createSequential : dispatcher : IDispatcher -> IScheduler
  val createParallel : maxTaskCount : int -> dispatcher : IDispatcher -> IScheduler
  val createMultithreaded
    : minThreadCount : int
    -> maxThreadCount : int
    -> dispatchers : IDispatcher list
    -> IScheduler
