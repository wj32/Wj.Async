namespace Wj.Async

open System.Collections.Generic;
open System.Threading;

module Dispatcher =
  // IDispatcher functions
  let enqueue (t : IDispatcher) f = t.Enqueue(f)
  let run (t : IDispatcher) f = t.Run(f)

  [<ReferenceEqualityAttribute>]
  type T =
    { queue : (unit -> unit) Queue;
      queueLock : obj; }

    interface IDispatcher with
      member t.Enqueue f =
        lock t.queueLock (fun () ->
          t.queue.Enqueue(f)
          Monitor.Pulse(t.queueLock)
        )

      member t.Run f =
        ThreadDispatcher.push (t :> IDispatcher)
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
          | Some f ->
            f ()
            loop ()
          | None ->
            d.Get()
        let result = loop ()
        ThreadDispatcher.pop (t :> IDispatcher)
        result

  let current () = ThreadDispatcher.current ()

  let tryCurrent () = ThreadDispatcher.tryCurrent ()

  let create () =
    { queue = new Queue<unit -> unit>();
      queueLock = new obj(); }
    :> IDispatcher
