namespace Wj.Async

open System
open System.Collections.Generic

type DeferredBuilder() =
  member this.Bind(t, f) = Deferred.bind t f

  member this.Combine(t, f) = Deferred.bind t f

  member this.Delay(f) = f

  member this.Return(x) = Deferred.value x

  member this.ReturnFrom(t) = t

  member this.Run(f) = f ()

  member this.Zero() = Deferred.unit

  member this.TryFinally(body, finalizer) =
    Supervisor.tryFinally body (fun () -> finalizer (); Deferred.unit)

  member this.TryWith(body, handler) =
    Supervisor.tryWith body handler Supervisor.AfterDetermined.Log

  member this.Using(disposable : #IDisposable, body) =
    // TODO: If disposable is a struct, it will get boxed in the check below. Find a way of handling
    // this.
    Supervisor.tryFinally
      (fun () -> body disposable)
      (fun () -> (if not (obj.ReferenceEquals(disposable, null)) then disposable.Dispose()); Deferred.unit)

  member this.While(guard, body) =
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

  member this.For(xs, body) = Deferred.Seq.iter Parallelism.Sequential body xs

  member this.For(xs, body) = DeferredSeq.iter body xs
