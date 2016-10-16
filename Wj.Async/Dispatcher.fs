namespace Wj.Async

open System
open System.Collections.Generic
open System.Threading
open Wj.Async.Internal

module Dispatcher =
  let [<Literal>] SchedulerNotFound = "The scheduler is not associated with this dispatcher."

  // IDispatcher functions
  let inline enqueue (t : IDispatcher) supervisedCallback = t.Enqueue(supervisedCallback)
  let inline run (t : IDispatcher) f = t.Run(f)
  let inline rootSupervisor (t : IDispatcher) = t.RootSupervisor
  let inline registerScheduler (t : IDispatcher) scheduler = t.RegisterScheduler(scheduler)

  [<Interface>]
  type 'a ISchedulerRegistrationManager =
    abstract member Update : 'a -> unit
    abstract member Remove : 'a -> unit

  type SchedulerRegistration =
    { manager : SchedulerRegistration ISchedulerRegistrationManager;
      scheduler : IScheduler;
      schedulerTake : ISchedulerTake;
      mutable node : SchedulerRegistration LinkedListNode; }

    interface ISchedulerRegistration with
      member t.Update() = t.manager.Update(t)

      member t.Remove() = t.manager.Remove(t)

  type TookFrom = Nothing = 0 | MainQueue = 1 | Scheduler = 2 | Result = 3

  [<ReferenceEquality>]
  type T =
    { mutable rootSupervisor : ISupervisor;
      lock : obj;
      mainQueue : unit SupervisedCallback Queue.T;
      schedulers : Dictionary<IScheduler, SchedulerRegistration>;
      readySchedulers : SchedulerRegistration LinkedList;
      idleSchedulers : SchedulerRegistration LinkedList;
      giveSchedulers : IScheduler HashSet; }

    member t.GetListForScheduler(registration) =
      if registration.schedulerTake.QueueDepth <> 0 then t.readySchedulers else t.idleSchedulers

    member t.RemoveSchedulerInternal(registration) =
      registration.scheduler.Dispatchers.Remove(t) |> ignore
      registration.node.List.Remove(registration.node)
      registration.node <- null

    interface IDisposable with
      member t.Dispose() =
        lock t.lock (fun () ->
          for registration in t.schedulers.Values do
            t.RemoveSchedulerInternal(registration)
          t.schedulers.Clear()
          for scheduler in t.giveSchedulers do
            scheduler.Dispatchers.Remove(t) |> ignore
          t.giveSchedulers.Clear()
        )

    interface IDispatcher with
      member t.Enqueue(supervisedCallback) =
        lock t.lock (fun () ->
          Queue.enqueue t.mainQueue supervisedCallback
          Monitor.Pulse(t.lock)
        )

      member t.Run(f) =
        let supervisor = t.rootSupervisor
        let emptySupervisedCallback = (supervisor, id)
        ThreadShared.pushSupervisor supervisor
        ThreadShared.pushDispatcher t
        try
          let d = f ()
          d.Upon(fun _ ->
            lock t.lock (fun () ->
              Monitor.Pulse(t.lock)
            )
          )
          let rec loop () =
            let tookFrom, (supervisor, f) = lock t.lock (fun () ->
              while Queue.isEmpty t.mainQueue && t.readySchedulers.Count = 0 && not d.IsDetermined do
                Monitor.Wait(t.lock) |> ignore
              if not (Queue.isEmpty t.mainQueue) then
                (TookFrom.MainQueue, Queue.dequeue t.mainQueue)
              else if t.readySchedulers.Count <> 0 then
                let registration = t.readySchedulers.First.Value
                t.readySchedulers.RemoveFirst()
                if registration.schedulerTake.QueueDepth <> 0 then
                  let supervisedCallback = registration.schedulerTake.Dequeue()
                  t.GetListForScheduler(registration).AddLast(registration.node)
                  (TookFrom.Scheduler, supervisedCallback)
                else
                  t.GetListForScheduler(registration).AddLast(registration.node)
                  (TookFrom.Nothing, emptySupervisedCallback)
              else
                (TookFrom.Result, emptySupervisedCallback)
            )
            match tookFrom with
            | TookFrom.Nothing -> loop ()
            | TookFrom.MainQueue | TookFrom.Scheduler ->
              match supervisor.TryRun(f) with
              | Result.Success () -> ()
              | Result.Failure ex -> supervisor.SendException(ex)
              loop ()
            | TookFrom.Result -> d.Get()
            | _ -> failwith "Unexpected TookFrom enum value"
          let result = loop ()
          result
        finally
          ThreadShared.popDispatcher t
          ThreadShared.popSupervisor supervisor

      member t.RootSupervisor = t.rootSupervisor

      member t.RegisterScheduler(scheduler) =
        match scheduler.Take with
        | Some schedulerTake ->
          let registration =
            {manager = t; scheduler = scheduler; schedulerTake = schedulerTake; node = null}
          registration.node <- new LinkedListNode<SchedulerRegistration>(registration)
          lock t.lock (fun () ->
            scheduler.Dispatchers.Add(t)
            t.schedulers.Add(scheduler, registration)
            t.GetListForScheduler(registration).AddLast(registration.node)
            if schedulerTake.QueueDepth <> 0 then
              Monitor.Pulse(t.lock)
          )
          registration :> ISchedulerRegistration
        | None ->
          let registration =
            { manager = t;
              scheduler = scheduler;
              schedulerTake = Unchecked.defaultof<ISchedulerTake>;
              node = null; }
          lock t.lock (fun () ->
            scheduler.Dispatchers.Add(t)
            t.giveSchedulers.Add(scheduler) |> ignore
          )
          registration :> ISchedulerRegistration

    interface SchedulerRegistration ISchedulerRegistrationManager with
      member t.Update(registration) =
        lock t.lock (fun () ->
          match t.schedulers.TryGetValue(registration.scheduler) with
          | true, registration ->
            let list = t.GetListForScheduler(registration)
            if not (obj.ReferenceEquals(registration.node.List, list)) then
              registration.node.List.Remove(registration.node)
              list.AddLast(registration.node)
              if registration.schedulerTake.QueueDepth <> 0 then
                Monitor.Pulse(t.lock)
          | _ -> ()
        )

      member t.Remove(registration) =
        lock t.lock (fun () ->
          match t.schedulers.TryGetValue(registration.scheduler) with
          | true, registration ->
            t.RemoveSchedulerInternal(registration)
            t.schedulers.Remove(registration.scheduler) |> ignore
          | _ ->
            if t.giveSchedulers.Remove(registration.scheduler) then
              registration.scheduler.Dispatchers.Remove(t) |> ignore
            else
              invalidOp SchedulerNotFound
        )

  let current () = ThreadShared.currentDispatcher ()

  let create () =
    let t =
      { rootSupervisor = Unchecked.defaultof<ISupervisor>;
        lock = new obj();
        mainQueue = Queue.create ();
        schedulers = new Dictionary<IScheduler, SchedulerRegistration>();
        readySchedulers = new LinkedList<SchedulerRegistration>();
        idleSchedulers = new LinkedList<SchedulerRegistration>();
        giveSchedulers = new HashSet<IScheduler>(); }
    t.rootSupervisor <- RootSupervisor.create t
    t :> IDispatcher

  let enqueueRoot t f = enqueue t (rootSupervisor t, f)
