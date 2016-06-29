﻿namespace Wj.Async

open System.Threading
open Wj.Async.Internal

module Dispatcher =
  // IDispatcher functions
  let inline enqueue (t : IDispatcher) supervisedCallback = t.Enqueue(supervisedCallback)
  let inline run (t : IDispatcher) f = t.Run(f)
  let inline rootSupervisor (t : IDispatcher) = t.RootSupervisor

  [<ReferenceEquality>]
  type T =
    { queue : unit SupervisedCallback Queue.T;
      queueLock : obj;
      mutable rootSupervisor : ISupervisor; }

    interface IDispatcher with
      member t.Enqueue(supervisedCallback) =
        lock t.queueLock (fun () ->
          Queue.enqueue t.queue supervisedCallback
          Monitor.Pulse(t.queueLock)
        )

      member t.Run(f) =
        let supervisor = t.rootSupervisor
        ThreadShared.pushSupervisor supervisor
        ThreadShared.pushDispatcher t
        try
          let d = f ()
          Deferred.upon d (fun _ ->
            lock t.queueLock (fun () ->
              Monitor.Pulse(t.queueLock)
            )
          )
          let rec loop () =
            let f = lock t.queueLock (fun () ->
              while Queue.isEmpty t.queue && not (Deferred.isDetermined d) do
                Monitor.Wait(t.queueLock) |> ignore
              Queue.tryDequeue t.queue
            )
            match f with
            | Some (supervisor, f) ->
              if not (Supervisor.isTerminated supervisor) then
                match Supervisor.tryRun supervisor f with
                | Result.Success () -> ()
                | Result.Failure ex -> Supervisor.sendException supervisor ex
              loop ()
            | None ->
              Deferred.get d
          let result = loop ()
          result
        finally
          ThreadShared.popDispatcher t
          ThreadShared.popSupervisor supervisor

      member t.RootSupervisor = t.rootSupervisor

  let current () = ThreadShared.currentDispatcher ()

  let create () =
    let t =
      { queue = Queue.create ();
        queueLock = new obj();
        rootSupervisor = Unchecked.defaultof<ISupervisor>; }
    t.rootSupervisor <- Supervisor.createRoot t
    t :> IDispatcher
