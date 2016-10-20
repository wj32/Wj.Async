namespace Wj.Async

open System

module Throttle =
  // IThrottle functions
  val enqueue : IThrottle -> f : (unit -> 'a IDeferred) -> 'a IDeferred

  module OnException =
    type T = Stop | Continue

  [<AbstractClass>]
  type QueueThrottle =
    new : onException : OnException.T -> QueueThrottle
    abstract member CanStartTask : unit -> bool
    abstract member OnStartingTask : unit -> unit
    abstract member OnCompletedTask : unit -> unit
    abstract member OnQueuedTask : unit -> unit
    member Aborted : bool with get, set
    member QueueLength : int with get
    member StartTasks : unit -> int
    interface IThrottle

  val sequentialThrottle : unit -> IThrottle
  val sequential : unit -> Parallelism.T

  val parallelThrottle : unit -> IThrottle
  val ``parallel`` : unit -> Parallelism.T

  val parallelAtMostThrottle : maxTasks : int -> onException : OnException.T -> IThrottle
  val parallelAtMost : maxTasks : int -> Parallelism.T

  val parallelAtMostStartedPerThrottle : maxTasks : int -> span : TimeSpan -> onException : OnException.T -> IThrottle
  val parallelAtMostStartedPer : maxTasks : int -> span : TimeSpan -> Parallelism.T

  val sequentialAtMostOneStartedPerThrottle : span : TimeSpan -> onException : OnException.T -> IThrottle
  val sequentialAtMostOneStartedPer : span : TimeSpan -> Parallelism.T
