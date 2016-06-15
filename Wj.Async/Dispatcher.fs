namespace Wj.Async

open System.Threading;

module Dispatcher =
  // IDispatcher functions
  let enqueue (t : IDispatcher) supervisedCallback = t.Enqueue(supervisedCallback)
  let run (t : IDispatcher) f = t.Run(f)
  let rootSupervisor (t : IDispatcher) = t.RootSupervisor

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
          d.Upon(fun _ ->
            lock t.queueLock (fun () ->
              Monitor.Pulse(t.queueLock)
            )
          )
          let rec loop () =
            let f = lock t.queueLock (fun () ->
              while Queue.isEmpty t.queue && not d.IsDetermined do
                Monitor.Wait(t.queueLock) |> ignore
              Queue.tryDequeue t.queue
            )
            match f with
            | Some (supervisor, f) ->
              match supervisor.TryRun(f) with
              | Result.Success () -> ()
              | Result.Failure ex -> Supervisor.sendException supervisor ex
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
      { queue = Queue.create ();
        queueLock = new obj();
        rootSupervisor = Unchecked.defaultof<ISupervisor>; }
    t.rootSupervisor <- Supervisor.createRoot t
    t :> IDispatcher
