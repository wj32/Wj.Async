namespace Wj.Async

module Parallelism =
  type T =
    | Sequential
    | Parallel
    | ParallelAtMost of taskCount : int
    | Multithreaded
    | MultithreadedAtMost of threadCount : int
    | MultithreadedExactly of threadCount : int
    | ScheduledBy of scheduler : IScheduler

  val createScheduler : T -> IScheduler
