namespace Wj.Async

module internal ChildSupervisor =
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
        t.handlers |> List.iter (fun (supervisor, handler) ->
          supervisor.Run(fun () -> handler ex) |> ignore
        )
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

      member t.Run(f) =
        ThreadShared.pushSupervisor t
        let result = Result.tryWith f
        ThreadShared.popSupervisor t
        match result with
        | Result.Failure ex -> (t :> ISupervisor).SendException(ex)
        | Result.Success _ -> ()
        result

  let create name =
    { name = name;
      dispatcher = ThreadShared.currentDispatcher ();
      parent = ThreadShared.tryCurrentSupervisor ();
      handlers = []; }
    :> ISupervisor
