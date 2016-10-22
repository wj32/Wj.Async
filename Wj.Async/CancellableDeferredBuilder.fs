namespace Wj.Async

open System
open System.Collections.Generic

module CancellableDeferredBuilder =
  type 'a T = 'a IDeferred
  type 'a M = 'a T

  type B(cancellation : Cancellation.T option) =
    member this.RaiseIfCancelled() = Cancellation.Option.raiseIfSet cancellation

    member inline this.Bind(t, f) = Deferred.bind t (fun x -> this.RaiseIfCancelled(); f x)

    member inline this.Combine(t : unit M, f) = Deferred.bind t (fun () -> this.RaiseIfCancelled(); f ())

    member inline this.Delay(f) = f

    member inline this.Return(x) = Deferred.value x

    member inline this.ReturnFrom(t) = t

    member inline this.Run(f) = this.RaiseIfCancelled(); f ()

    member inline this.Zero() = Deferred.unit

    member inline this.TryFinally(body, finalizer) =
      Supervisor.tryFinally (fun () -> this.RaiseIfCancelled(); body ()) finalizer

    member inline this.TryFinally(body, finalizer) =
      Supervisor.tryFinally (fun () -> this.RaiseIfCancelled(); body ()) (fun () -> finalizer (); Deferred.unit)

    member inline this.TryWith(body, handler) =
      let supervisor = Supervisor.current ()
      // Bypass the handler for operation cancelled exceptions and send them directly to the current
      // supervisor.
      Supervisor.tryWith (fun () -> this.RaiseIfCancelled(); body ()) (fun ex ->
        match ex with
        | :? OperationCanceledException -> Supervisor.sendException supervisor ex; Deferred.never ()
        | _ -> this.RaiseIfCancelled(); handler ex
      ) Supervisor.AfterDetermined.Log

    member inline this.Using(disposable : #IDisposable, body) =
      // TODO: If disposable is a struct, it will get boxed in the check below. Find a way of handling
      // this.
      Supervisor.tryFinally
        (fun () -> this.RaiseIfCancelled(); body disposable)
        (fun () -> (if not (obj.ReferenceEquals(disposable, null)) then disposable.Dispose()); Deferred.unit)

    member inline this.WhileGeneric(upon, bind, guard, body : unit -> unit M) =
      bind (guard ()) (fun b ->
        if b then
          let v = Deferred.createVar ()
          let rec loop () =
            this.RaiseIfCancelled()
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
      this.WhileGeneric(Deferred.upon, Deferred.bind, (fun () -> this.RaiseIfCancelled(); guard ()), body)

    member inline this.For(xs, body) =
      Deferred.Seq.iter Parallelism.sequential (fun x -> this.RaiseIfCancelled(); body x) xs

    member inline this.For(xs, body) =
      DeferredSeq.iter' (fun x -> this.RaiseIfCancelled(); body x) xs
