namespace Wj.Async

module internal RootSupervisor =
  let [<Literal>] CannotAddHandlerToRoot = "Handlers cannot be registered on the root supervisor."
  let [<Literal>] RootCannotBeDetached = "The root supervisor cannot be detached."

  [<ReferenceEquality>]
  type T =
    | Root of dispatcher : IDispatcher

    interface ISupervisor with
      member t.Dispatcher = match t with Root dispatcher -> dispatcher

      member t.Parent = None

      member t.Name = "Root"

      member t.SendException(ex) =
        match t with
        | Root dispatcher ->
          if obj.ReferenceEquals(ThreadShared.currentDispatcher(), dispatcher) then
            raise (SupervisorRootException ex)
          else
            dispatcher.Enqueue((t :> ISupervisor, fun () -> raise (SupervisorRootException ex)))

      member t.Detach() = raise (invalidOp RootCannotBeDetached)

      member t.UponException(handler : exn -> unit) : unit =
        invalidOp CannotAddHandlerToRoot

      member t.UponException(supervisedHandler : exn SupervisedCallback) : unit =
        invalidOp CannotAddHandlerToRoot

      member t.TryRun(f) =
        ThreadShared.pushSupervisor t
        let result = Result.tryWith f
        ThreadShared.popSupervisor t
        result

  let inline create dispatcher = Root dispatcher
