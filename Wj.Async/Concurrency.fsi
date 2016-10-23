namespace Wj.Async

open System

module Concurrency =
  // IConcurrency functions
  val enqueue : IConcurrency -> f : (unit -> 'a IDeferred) -> 'a IDeferred

  module OnException =
    type T = Stop | Continue

  [<AbstractClass>]
  type QueueConcurrency =
    new : onException : OnException.T -> QueueConcurrency
    abstract member CanStartTask : unit -> bool
    abstract member OnStartingTask : unit -> unit
    abstract member OnCompletedTask : unit -> unit
    abstract member OnQueuedTask : unit -> unit
    member Aborted : bool with get, set
    member QueueLength : int
    member StartTasks : unit -> int

    interface IConcurrency

  val sequential : IConcurrency
  val concurrent : IConcurrency
  val createSequential : unit -> IConcurrency

  val concurrentAtMost' : maxTasks : int -> onException : OnException.T -> IConcurrency
  val concurrentAtMost : maxTasks : int -> IConcurrency

  val concurrentAtMostStartedPer' : maxTasks : int -> span : TimeSpan -> onException : OnException.T -> IConcurrency
  val concurrentAtMostStartedPer : maxTasks : int -> span : TimeSpan -> IConcurrency

  val sequentialAtMostOneStartedPer': span : TimeSpan -> onException : OnException.T -> IConcurrency
  val sequentialAtMostOneStartedPer : span : TimeSpan -> IConcurrency
