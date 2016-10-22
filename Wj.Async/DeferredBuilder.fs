namespace Wj.Async

open System
open System.Collections.Generic

module DeferredBuilder =
  type 'a T = 'a IDeferred
  type 'a M = 'a T

  type B() =
    member inline this.Bind(t, f) = Deferred.bind t f

    member inline this.Combine(t : unit M, f) = Deferred.bind t f

    member inline this.Delay(f) = f

    member inline this.Return(x) = Deferred.value x

    member inline this.ReturnFrom(t) = t

    member inline this.Run(f) = f ()

    member inline this.Zero() = Deferred.unit

    member inline this.TryFinally(body, finalizer) =
      Supervisor.tryFinally body finalizer

    member inline this.TryFinally(body, finalizer) =
      Supervisor.tryFinally body (fun () -> finalizer (); Deferred.unit)

    member inline this.TryWith(body, handler) =
      Supervisor.tryWith body handler Supervisor.AfterDetermined.Log

    member inline this.Using(disposable : #IDisposable, body) =
      // TODO: If disposable is a struct, it will get boxed in the check below. Find a way of handling
      // this.
      Supervisor.tryFinally
        (fun () -> body disposable)
        (fun () -> (if not (obj.ReferenceEquals(disposable, null)) then disposable.Dispose()); Deferred.unit)

    member inline this.WhileGeneric(upon, bind, guard, body : unit -> unit M) =
      bind (guard ()) (fun b ->
        if b then
          let v = Deferred.createVar ()
          let rec loop () =
            Deferred.upon (body ()) (fun () ->
              upon (guard ()) (fun b -> if b then loop () else Deferred.set v ())
            )
          loop ()
          v :> _ IDeferred
        else
          Deferred.unit
      )

    member inline this.While(guard, body : unit -> unit M) =
      this.WhileGeneric((|>), (|>), guard, body)

    member inline this.While(guard : unit -> bool IDeferred, body : unit -> unit M) =
      this.WhileGeneric(Deferred.upon, Deferred.bind, guard, body)

    member inline this.For(xs, body) = Deferred.Seq.iter Parallelism.sequential body xs

    member inline this.For(xs, body) = DeferredSeq.iter' body xs
