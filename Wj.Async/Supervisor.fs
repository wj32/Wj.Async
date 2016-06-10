namespace Wj.Async

open System

module Supervisor =
  let [<Literal>] private cannotAddHandlerToRoot =
    "Handlers cannot be registered on the root supervisor."

  // ISupervisor functions
  let dispatcher (t : ISupervisor) = t.Dispatcher
  let parent (t : ISupervisor) = t.Parent
  let name (t : ISupervisor) = t.Name
  let detach (t : ISupervisor) = t.Detach()
  let sendException (t : ISupervisor) ex = t.SendException(ex)
  let uponException (t : ISupervisor) (handler : exn -> unit) = t.UponException(handler)
  let uponException' (t : ISupervisor) (supervisedHandler : exn SupervisedCallback) =
    t.UponException(supervisedHandler)
  let run (t : ISupervisor) f = t.Run(f)

  module Child = ChildSupervisor

  module private Root =
    type T =
      | Root of dispatcher : IDispatcher

      interface ISupervisor with
        member t.Dispatcher = match t with Root dispatcher -> dispatcher

        member t.Parent = None

        member t.Name = "Root"

        member t.SendException(ex) = raise ex

        member t.Detach() = ()

        member t.UponException(handler : exn -> unit) : unit =
          invalidOp cannotAddHandlerToRoot

        member t.UponException(supervisedHandler : exn SupervisedCallback) : unit =
          invalidOp cannotAddHandlerToRoot

        member t.Run(f) = Result.tryWith f

    let inline create dispatcher = Root dispatcher

  let current () = ThreadShared.currentSupervisor ()

  let create () = Child.create "Unnamed supervisor"

  let createNamed name = Child.create name

  let createRoot dispatcher = Root.create dispatcher :> ISupervisor

  let getExceptionDSeq t =
    DeferredSeq.create (fun writer -> uponException t (DeferredSeq.Writer.write writer))

  let supervise (f : unit -> _ IDeferred) observer =
    let t = createNamed "Supervisor.supervise"
    uponException t observer
    let result = run t f
    match result with
    | Result.Success d -> d
    | Result.Failure _ -> Deferred.never ()

  module AfterDetermined =
    type T = Raise | Log | Ignore

  let handleAfterDetermined afterDetermined ex =
    match afterDetermined with
    | AfterDetermined.Raise -> raise ex
    | AfterDetermined.Log -> stderr.WriteLine(sprintf "Unhandled exception after tryWith:\n%s" (string ex))
    | AfterDetermined.Ignore -> ()

  let tryWith (f : unit -> _ IDeferred) (handler : exn -> _ IDeferred) afterDetermined =
    let t = createNamed "tryWith"
    detach t
    let startHandlingAfterDetermined () =
      uponException t (fun ex -> handleAfterDetermined afterDetermined ex)
    let result = run t f
    match result with
    | Result.Success d ->
      if Deferred.isDetermined d then
        startHandlingAfterDetermined ()
        d
      else
        let dispatcher = ThreadShared.currentDispatcher ()
        let reader = Deferred.createVar ()
        let mutable writer = Some reader
        uponException t (fun ex ->
          match writer with
          | Some v ->
            writer <- None
            Deferred.link v (handler ex)
          | None -> handleAfterDetermined afterDetermined ex
        )
        Deferred.upon' d (dispatcher.RootSupervisor, (fun x ->
          match writer with
          | Some v ->
            writer <- None
            Deferred.set v x
          | None -> ()
        ))
        reader :> _ IDeferred
    | Result.Failure ex ->
      startHandlingAfterDetermined ()
      handler ex

  let tryFinally f finalizer = Deferred.tryFinally f finalizer
