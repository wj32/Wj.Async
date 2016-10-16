namespace Wj.Async

open System

module Supervisor =
  // ISupervisor functions
  let inline dispatcher (t : ISupervisor) = t.Dispatcher
  let inline parent (t : ISupervisor) = t.Parent
  let inline name (t : ISupervisor) = t.Name
  let inline detach (t : ISupervisor) = t.Detach()
  let inline sendException (t : ISupervisor) ex = t.SendException(ex)
  let inline uponException (t : ISupervisor) (handler : exn -> unit) = t.UponException(handler)
  let inline uponException' (t : ISupervisor) (supervisedHandler : exn SupervisedCallback) =
    t.UponException(supervisedHandler)
  let inline tryRun (t : ISupervisor) f = t.TryRun(f)

  let current () = ThreadShared.currentSupervisor ()

  let create () = ChildSupervisor.create "Unnamed supervisor"

  let createNamed name = ChildSupervisor.create name

  let getExceptionDSeq t =
    DeferredSeq.create (fun writer -> uponException t (DeferredSeq.Writer.write writer))

  let supervise (f : unit -> _ IDeferred) observer =
    let t = createNamed "supervise"
    uponException t observer
    let result = tryRun t f
    match result with
    | Result.Success d -> d
    | Result.Failure ex -> sendException t ex; Deferred.never ()

  module AfterDetermined =
    type T = Raise | Log | Ignore

  let handleAfterDetermined afterDetermined supervisorName ex =
    match afterDetermined with
    | AfterDetermined.Raise -> raise (AfterDeterminedException (supervisorName, ex))
    | AfterDetermined.Log -> stderr.WriteLine(sprintf "Unhandled exception after tryWith:\n%s" (string ex))
    | AfterDetermined.Ignore -> ()

  let tryWith (f : unit -> _ IDeferred) (handler : exn -> _ IDeferred) afterDetermined =
    let dispatcher = ThreadShared.currentDispatcher ()
    let t = createNamed "tryWith"
    detach t
    let startHandlingAfterDetermined () =
      // Use the root supervisor whenever possible to avoid memory leaks.
      let supervisorForAfterDetermined =
        match afterDetermined with
        | AfterDetermined.Raise -> current ()
        | AfterDetermined.Log -> Dispatcher.rootSupervisor dispatcher
        | AfterDetermined.Ignore -> Dispatcher.rootSupervisor dispatcher
      uponException' t
        (supervisorForAfterDetermined, fun ex -> handleAfterDetermined afterDetermined t.Name ex)
    let result = tryRun t f
    match result with
    | Result.Success d ->
      if Deferred.isDetermined d then
        startHandlingAfterDetermined ()
        d
      else
        let reader = Deferred.createVar ()
        let mutable writer = Some reader
        uponException t (fun ex ->
          match writer with
          | Some v -> writer <- None; Deferred.link v (handler ex)
          | None -> handleAfterDetermined afterDetermined t.Name ex
        )
        Deferred.upon' d (Dispatcher.rootSupervisor dispatcher, fun x ->
          match writer with
          | Some v -> writer <- None; Deferred.set v x
          | None -> ()
        )
        reader :> _ IDeferred
    | Result.Failure ex ->
      startHandlingAfterDetermined ()
      handler ex

  let tryFinally f finalizer = Deferred.tryFinally f finalizer
