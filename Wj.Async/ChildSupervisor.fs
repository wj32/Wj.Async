namespace Wj.Async

module internal ChildSupervisor =
  [<ReferenceEquality>]
  type T =
    { name : string;
      mutable dispatcher : IDispatcher;
      mutable parent : ISupervisor option;
      mutable handlers : exn SupervisedCallback list; }

    interface ISupervisor with
      member t.Dispatcher = t.dispatcher

      member t.Parent = t.parent

      member t.Name = t.name

      member t.SendException(ex) =
        for (supervisor, handler) in t.handlers do
          match supervisor.TryRun(fun () -> handler ex) with
          | Result.Success () -> ()
          | Result.Failure ex -> supervisor.SendException(ex)
        match t.parent with
        | Some parent ->
          let ex' =
            match ex with
            | SupervisorChildException (supervisorNames, _) ->
              SupervisorChildException (t.name :: supervisorNames, ex)
            | ex ->
              SupervisorChildException ([t.name], ex)
          parent.SendException(ex')
        | None -> ()

      member t.Detach() = t.parent <- None

      member t.UponException(handler) =
        (t :> ISupervisor).UponException((ThreadShared.currentSupervisor (), handler))

      member t.UponException(supervisedHandler) =
        t.handlers <- supervisedHandler :: t.handlers

      member t.TryRun(f) =
        ThreadShared.pushSupervisor t
        let result = Result.tryWith f
        ThreadShared.popSupervisor t
        result

  let inline create name =
    { name = name;
      dispatcher = ThreadShared.currentDispatcher ();
      parent = ThreadShared.tryCurrentSupervisor ();
      handlers = []; }
    :> ISupervisor
