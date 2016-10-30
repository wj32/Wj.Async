namespace Wj.Async

open System
open Wj.Async.Internal

module Concurrency =
  open Deferred.Infix

  let [<Literal>] SpanCannotBeNegative = "The time span value cannot be negative."

  // IConcurrency functions
  let enqueue (t : IConcurrency) f = t.Enqueue(f)

  module OnException =
    type T = Stop | Continue

  [<AbstractClass>]
  type QueueConcurrency private(uponException : exn -> unit) =
    let childSupervisor = ChildSupervisor.create "QueueConcurrency"
    let tasks = Queue.create ()

    do
      childSupervisor.Detach()
      childSupervisor.UponException(uponException)

    abstract member CanStartTask : unit -> bool
    abstract member OnStartingTask : unit -> unit
    abstract member OnCompletedTask : unit -> unit
    abstract member OnQueuedTask : unit -> unit

    member val Aborted = false with get, set
    member val private Tasks = Queue.create () with get
    member this.QueueLength = Queue.length this.Tasks

    new(onException) as this =
      let supervisor = ThreadShared.currentSupervisor ()
      QueueConcurrency(fun ex ->
        match onException with
        | OnException.Stop ->
          if not this.Aborted then
            this.Aborted <- true
            Queue.clear this.Tasks
        | OnException.Continue -> ()
        supervisor.SendException(ex)
      )

    member inline private this.StartTask(f, failure) =
      this.OnStartingTask()
      try
        Supervisor.run childSupervisor f
      with ex ->
        Supervisor.sendException childSupervisor ex; failure ()

    member this.StartTasks() =
      let mutable count = 0
      while not (Queue.isEmpty this.Tasks) && this.CanStartTask() do
        this.StartTask(Queue.dequeue this.Tasks, id)
        count <- count + 1
      count

    interface IConcurrency with
      member this.Enqueue(f) =
        if not this.Aborted then
          if Queue.isEmpty this.Tasks && this.CanStartTask() then
            this.StartTask((fun () ->
              let d = f ()
              d >>> (fun _ -> this.OnCompletedTask())
              d
            ), Deferred.never)
          else
            Deferred.create (fun v ->
              Queue.enqueue this.Tasks (fun () ->
                f () >>> (fun x -> v <-- x; this.OnCompletedTask())
              )
            )
        else
          Deferred.never ()

  type ConcurrentAtMostConcurrency(maxTasks, onException) =
    inherit QueueConcurrency(onException)

    let mutable currentTasks = 0

    override this.CanStartTask() = currentTasks < maxTasks

    override this.OnStartingTask() = currentTasks <- currentTasks + 1

    override this.OnCompletedTask() = currentTasks <- currentTasks - 1; this.StartTasks() |> ignore

    override this.OnQueuedTask() = ()

  type ConcurrentAtMostStartedPerConcurrency(maxTasks, span : TimeSpan, onException) =
    inherit QueueConcurrency(onException)

    let stopwatch = new System.Diagnostics.Stopwatch()
    let timestamps = Queue.create ()

    do
      if span.Ticks < 0L then invalidArg "span" SpanCannotBeNegative
      stopwatch.Start()

    override this.CanStartTask() = Queue.length timestamps < maxTasks

    override this.OnStartingTask() =
      let now = stopwatch.Elapsed
      Queue.enqueue timestamps now
      if Queue.length timestamps = maxTasks then
        let diff = now - Queue.first timestamps
        if diff >= span then
          Queue.dequeue timestamps |> ignore
        else
          Clock.after diff
          >>> fun () ->
          Queue.dequeue timestamps |> ignore
          this.StartTasks() |> ignore

    override this.OnCompletedTask() = ()

    override this.OnQueuedTask() = ()

  type SequentialAtMostOneStartedPerConcurrency(span : TimeSpan, onException) =
    inherit QueueConcurrency(onException)

    let stopwatch = new System.Diagnostics.Stopwatch()
    let mutable busy = false
    let mutable startedTime = -span

    do
      if span.Ticks < 0L then invalidArg "span" SpanCannotBeNegative
      stopwatch.Start()

    member this.Schedule() =
      let diff = startedTime + span - stopwatch.Elapsed
      (if diff.Ticks > 0L then
        Clock.after diff
      else
        Deferred.unit)
      >>> fun () ->
      // Force the next task to start.
      startedTime <- -span
      this.StartTasks() |> ignore

    override this.CanStartTask() = not busy && stopwatch.Elapsed >= startedTime + span

    override this.OnStartingTask() =
      busy <- true
      startedTime <- stopwatch.Elapsed

    override this.OnCompletedTask() =
      busy <- false
      if this.StartTasks() = 0 then
        if this.QueueLength <> 0 then
          this.Schedule()

    override this.OnQueuedTask() =
      if not busy then
        this.Schedule()

  let sequential = Deferred.LocallySequentialConcurrency.Unique :> IConcurrency

  let concurrent = Deferred.ConcurrentConcurrency.Unique :> IConcurrency

  let createSequential () = new Deferred.SequentialConcurrency() :> IConcurrency

  let concurrentAtMost' maxTasks onException =
    ConcurrentAtMostConcurrency(maxTasks, onException) :> IConcurrency

  let concurrentAtMost maxTasks = concurrentAtMost' maxTasks OnException.Stop

  let concurrentAtMostStartedPer' maxTasks span onException =
    ConcurrentAtMostStartedPerConcurrency(maxTasks, span, onException) :> IConcurrency

  let concurrentAtMostStartedPer maxTasks span =
    concurrentAtMostStartedPer' maxTasks span OnException.Stop

  let sequentialAtMostOneStartedPer' span onException =
    SequentialAtMostOneStartedPerConcurrency(span, onException) :> IConcurrency

  let sequentialAtMostOneStartedPer span = sequentialAtMostOneStartedPer' span OnException.Stop
