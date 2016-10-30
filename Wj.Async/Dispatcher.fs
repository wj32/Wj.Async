namespace Wj.Async

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
        let rootSupervisor = t.rootSupervisor
        let mutable stop = false
        ThreadShared.pushSupervisor rootSupervisor
        ThreadShared.pushDispatcher t
        try
          let d =
            try
              f ()
            with ex ->
              rootSupervisor.SendException(ex)
              Deferred.never ()
          Deferred.upon d (fun _ ->
            lock t.queueLock (fun () ->
              Monitor.Pulse(t.queueLock)
            )
          )
          let rec loop () =
            let supervisedCallback = lock t.queueLock (fun () ->
              while Queue.isEmpty t.queue && not (Deferred.isDetermined d) do
                Monitor.Wait(t.queueLock) |> ignore
              if Queue.isEmpty t.queue then
                stop <- true
                Unchecked.defaultof<unit SupervisedCallback>
              else
                Queue.dequeue t.queue
            )
            if stop then
              Deferred.get d
            else
              let supervisor, f = supervisedCallback
              try
                supervisor.Run(f)
              with ex ->
                supervisor.SendException(ex)
              loop ()
          let result = loop ()
          result
        finally
          ThreadShared.popDispatcher t
          ThreadShared.popSupervisor rootSupervisor

      member t.RootSupervisor = t.rootSupervisor

  let current () = ThreadShared.currentDispatcher ()

  let create () =
    let t =
      { queue = Queue.create ();
        queueLock = new obj();
        rootSupervisor = Unchecked.defaultof<ISupervisor>; }
    t.rootSupervisor <- RootSupervisor.create t
    t :> IDispatcher

  let enqueue' t supervisor (f : unit -> _ IDeferred) =
    let rootSupervisor = rootSupervisor (current ())
    Deferred.create (fun v ->
      enqueue t (supervisor, fun () ->
        Deferred.upon' (f ()) (rootSupervisor, fun x -> Deferred.set v x)
      )
    )

  let enqueueRoot t f = enqueue t (rootSupervisor t, f)
