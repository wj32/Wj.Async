﻿namespace Wj.Async

open System.Collections.Generic;
open System.Threading;

module Dispatcher =
  // IDispatcher functions
  let enqueue (t : IDispatcher) supervisedCallback = t.Enqueue(supervisedCallback)
  let run (t : IDispatcher) f = t.Run(f)
  let rootSupervisor (t : IDispatcher) = t.RootSupervisor

  [<ReferenceEquality>]
  type T =
    { queue : unit SupervisedCallback Queue;
      queueLock : obj;
      mutable rootSupervisor : ISupervisor; }

    interface IDispatcher with
      member t.Enqueue(supervisedCallback) =
        lock t.queueLock (fun () ->
          t.queue.Enqueue(supervisedCallback)
          Monitor.Pulse(t.queueLock)
        )

      member t.Run(f) =
        let supervisor = t.rootSupervisor
        ThreadShared.pushSupervisor supervisor
        ThreadShared.pushDispatcher t
        try
          let d = f ()
          d.Upon(fun _ ->
            lock t.queueLock (fun () ->
              Monitor.Pulse(t.queueLock)
            )
          )
          let rec loop () =
            let f = lock t.queueLock (fun () ->
              while t.queue.Count = 0 && not d.IsDetermined do
                Monitor.Wait(t.queueLock) |> ignore
              if t.queue.Count <> 0 then
                Some (t.queue.Dequeue())
              else
                None
            )
            match f with
            | Some (supervisor, f) ->
              supervisor.Run(f) |> ignore
              loop ()
            | None ->
              d.Get()
          let result = loop ()
          result
        finally
          ThreadShared.popDispatcher t
          ThreadShared.popSupervisor supervisor

      member t.RootSupervisor = t.rootSupervisor

  let current () = ThreadShared.currentDispatcher ()

  let create () =
    let t =
      { queue = new Queue<ISupervisor * (unit -> unit)>();
        queueLock = new obj();
        rootSupervisor = Unchecked.defaultof<ISupervisor>; }
    t.rootSupervisor <- Supervisor.createRoot t
    t :> IDispatcher
