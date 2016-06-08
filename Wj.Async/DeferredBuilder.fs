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
    Supervisor.tryFinally
      (fun () -> body disposable)
      (fun () -> (if not (isNull disposable) then disposable.Dispose()); Deferred.unit)

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

  member this.For(xs : _ seq, body : _ -> unit IDeferred) =
    this.Using(
      xs.GetEnumerator(),
      fun e ->
        if e.MoveNext() then
          let v = Deferred.createVar ()
          let rec loop x =
            Deferred.upon (body x) (fun () ->
              if e.MoveNext() then
                loop e.Current
              else
                Deferred.set v ()
            )
          loop e.Current
          v :> _ IDeferred
        else
          Deferred.unit
    )
