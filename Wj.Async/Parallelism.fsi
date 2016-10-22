namespace Wj.Async

open System

module Parallelism =
  // IParallelism functions
  val enqueue : IParallelism -> f : (unit -> 'a IDeferred) -> 'a IDeferred

  module OnException =
    type T = Stop | Continue

  [<AbstractClass>]
  type QueueParallelism =
    new : onException : OnException.T -> QueueParallelism
    abstract member CanStartTask : unit -> bool
    abstract member OnStartingTask : unit -> unit
    abstract member OnCompletedTask : unit -> unit
    abstract member OnQueuedTask : unit -> unit
    member Aborted : bool with get, set
    member QueueLength : int with get
    member StartTasks : unit -> int
    interface IParallelism

  val inline sequential : unit -> IParallelism

  val inline ``parallel`` : unit -> IParallelism

  val parallelAtMost' : maxTasks : int -> onException : OnException.T -> IParallelism
  val parallelAtMost : maxTasks : int -> IParallelism

  val parallelAtMostStartedPer' : maxTasks : int -> span : TimeSpan -> onException : OnException.T -> IParallelism
  val parallelAtMostStartedPer : maxTasks : int -> span : TimeSpan -> IParallelism

  val sequentialAtMostOneStartedPer': span : TimeSpan -> onException : OnException.T -> IParallelism
  val sequentialAtMostOneStartedPer : span : TimeSpan -> IParallelism
