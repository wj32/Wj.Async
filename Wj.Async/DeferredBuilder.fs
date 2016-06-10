namespace Wj.Async

open System
open System.Collections.Generic

type DeferredBuilder() =
  member inline this.Bind(t, f) = Deferred.bind t f

  member inline this.Combine(t, f) = Deferred.bind t f

  member inline this.Delay(f) = f

  member inline this.Return(x) = Deferred.value x

  member inline this.ReturnFrom(t) = t

  member inline this.Run(f) = f ()

  member inline this.Zero() = Deferred.unit

  member inline this.TryFinally(body, finalizer) =
    Supervisor.tryFinally body finalizer

  member inline this.TryWith(body, handler) =
    Supervisor.tryWith body handler Supervisor.AfterDetermined.Log

  member inline this.Using(disposable : #IDisposable, body) =
    // TODO: If disposable is a struct, it will get boxed in the check below. Find a way of handling
    // this.
    Supervisor.tryFinally
      (fun () -> body disposable)
      (fun () -> (if not (obj.ReferenceEquals(disposable, null)) then disposable.Dispose()); Deferred.unit)

  member this.While(guard, body : unit -> unit IDeferred) =
    if guard () then
      let v = Deferred.createVar ()
      let rec loop () =
        Deferred.upon (body ()) (fun () ->
          if guard () then
            loop ()
          else
            Deferred.set v ()
        )
      loop ()
      v :> _ IDeferred
    else
      Deferred.unit

  member inline this.For(xs, body) = Deferred.Seq.iter Parallelism.Sequential body xs

  member inline this.For(xs, body) = DeferredSeq.iter' body xs
