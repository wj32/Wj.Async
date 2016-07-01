namespace Wj.Async

open System

module DeferredSeqBuilder =
  open Deferred.Infix

  type 'a T = 'a DeferredSeq.T
  type 'a M = 'a DeferredSeq.Writer.T -> unit IDeferred

  type B() =
    member inline this.Bind(d, f : _ -> _ M) = fun writer -> d >>= (fun x -> f x writer)

    member inline this.Combine(m1 : _ M, m2 : _ M) = fun writer -> m1 writer >>= fun () -> m2 writer

    member inline this.Delay(f : unit -> _ M) = fun writer -> f () writer

    member inline this.Run(m) = DeferredSeq.create' m

    member inline this.Yield(x) = fun writer -> DeferredSeq.Writer.write writer x; Deferred.unit

    member inline this.YieldFrom(xs) = fun writer -> xs |> DeferredSeq.iter (DeferredSeq.Writer.write writer)

    member inline this.Zero() : _ M = fun writer -> Deferred.unit

    member inline this.TryFinally(body : _ M, finalizer) = fun writer ->
      Supervisor.tryFinally (fun () -> body writer) finalizer

    member inline this.TryFinally(body : _ M, finalizer) = fun writer ->
      Supervisor.tryFinally (fun () -> body writer) (fun () -> finalizer (); Deferred.unit)

    member inline this.TryWith(body : _ M, handler) = fun writer ->
      Supervisor.tryWith (fun () -> body writer) (fun ex -> handler ex writer) Supervisor.AfterDetermined.Log

    member inline this.Using(disposable : #IDisposable, body : _ -> _ M) = fun writer ->
      // TODO: If disposable is a struct, it will get boxed in the check below. Find a way of handling
      // this.
      Supervisor.tryFinally
        (fun () -> body disposable writer)
        (fun () -> (if not (obj.ReferenceEquals(disposable, null)) then disposable.Dispose()); Deferred.unit)

    member inline this.WhileGeneric(upon, bind, guard, body : _ M) = fun writer ->
      bind (guard ()) (fun b ->
        if b then
          let v = Deferred.createVar ()
          let rec loop () =
            body writer >>> (fun () ->
              upon (guard ()) (fun b -> if b then loop () else v <-- ())
            )
          loop ()
          v :> _ IDeferred
        else
          Deferred.unit
      )

    member inline this.While(guard, body) =
      this.WhileGeneric((|>), (|>), guard, body)

    member inline this.While(guard : unit -> bool IDeferred, body) =
      this.WhileGeneric(Deferred.upon, Deferred.bind, guard, body)

    member inline this.For(xs, body : _ -> _ M) = fun writer ->
      Deferred.Seq.iter Parallelism.Sequential (fun x -> body x writer) xs

    member inline this.For(xs, body : _ -> _ M) = fun writer ->
      DeferredSeq.iter' (fun x -> body x writer) xs
