namespace Wj.Async

open System

exception SupervisorChildException of string list * Exception

module Supervisor =
  let [<Literal>] private cannotAddHandlerToRoot =
    "Handlers cannot be registered on the root supervisor."

  let systemRaise = raise

  // ISupervisor functions
  let parent (t : ISupervisor) = t.Parent
  let name (t : ISupervisor) = t.Name
  let raise (t : ISupervisor) ex = t.Raise(ex)
  let detach (t : ISupervisor) = t.Detach()
  let uponException (t : ISupervisor) handler = t.UponException(handler)
  let uponException' (t : ISupervisor) (supervisor, handler) = t.UponException(supervisor, handler)
  let run (t : ISupervisor) f = t.Run(f)

  module Child =
    type T =
      { name : string;
        mutable parent : ISupervisor option;
        mutable handlers : (ISupervisor * (Exception -> unit)) list; }

      interface ISupervisor with
        member t.Parent = t.parent

        member t.Name = t.name

        member t.Raise(ex) =
          t.handlers |> List.iter (fun (supervisor, handler) -> supervisor.Run(fun () -> handler ex))
          match t.parent with
          | Some parent ->
            let ex' =
              match ex with
              | SupervisorChildException (supervisorNames, _) ->
                SupervisorChildException (t.name :: supervisorNames, ex)
              | ex ->
                SupervisorChildException ([], ex)
            parent.Raise(ex')
          | None -> ()

        member t.Detach() = t.parent <- None

        member t.UponException(handler) =
          (t :> ISupervisor).UponException(ThreadShared.currentSupervisor (), handler)

        member t.UponException(supervisor, handler) =
          t.handlers <- (supervisor, handler) :: t.handlers

        member t.Run(f) =
          ThreadShared.pushSupervisor t
          let mutable poppedSupervisor = false
          try
            f ()
          with ex ->
            ThreadShared.popSupervisor t
            poppedSupervisor <- true
            (t :> ISupervisor).Raise(ex)
          if not poppedSupervisor then
            ThreadShared.popSupervisor t

    let create name =
      { name = name;
        parent = ThreadShared.tryCurrentSupervisor ();
        handlers = []; }
      :> ISupervisor

  module Root =
    type T =
      | Root

      interface ISupervisor with
        member t.Parent = None

        member t.Name = "Root"

        member t.Raise(ex) = systemRaise ex

        member t.Detach() = ()

        member t.UponException(handler) = invalidOp cannotAddHandlerToRoot

        member t.UponException(supervisor, handler) = invalidOp cannotAddHandlerToRoot

        member t.Run(f) = f ()

  let root = Root.Root :> ISupervisor

  let current () = ThreadShared.currentSupervisor ()

  let create () = Child.create "Unnamed supervisor"

  let createNamed name = Child.create name

  let supervise (f : unit -> _ IDeferred) observer =
    let v = Deferred.createNode ()
    let t = createNamed "Supervisor.supervise"
    uponException t observer
    run t (fun () -> Deferred.link v (f ()))
    v :> _ IDeferred

  module AfterDetermined =
    type T = Raise | Log | Ignore

    let dontRaise t = if t = Raise then Log else t

  let raiseAfterDetermined afterDetermined ex =
    match afterDetermined with
    | AfterDetermined.Raise -> systemRaise ex
    | AfterDetermined.Log -> stderr.WriteLine(sprintf "Unhandled exception after tryWith:\n%s" (string ex))
    | AfterDetermined.Ignore -> ()

  let tryWith' name (f : unit -> _ IDeferred) (handler : Exception -> _ IDeferred) afterDetermined =
    let v = Deferred.createVar ()
    let mutable exceptionOccurred = false
    let t = createNamed name
    detach t
    uponException t (fun ex ->
      if Deferred.isDetermined v then
        raiseAfterDetermined afterDetermined ex
      else
        if exceptionOccurred then
          raiseAfterDetermined (AfterDetermined.dontRaise afterDetermined) ex
        else
          exceptionOccurred <- true
          Deferred.upon (handler ex) (fun x -> Deferred.set v x)
    )
    run t (fun () ->
      Deferred.upon (f ()) (fun x ->
        if not exceptionOccurred then
          Deferred.set v x
      )
    )
    v :> _ IDeferred

  let tryWith f handler afterDetermined =
    tryWith' "Supervisor.tryWith" f handler afterDetermined

  module Result =
    type 'a T =
    | Result of 'a
    | Exception of Exception

  let tryFinally (f : unit -> _ IDeferred) (finalizer : unit -> _ IDeferred) =
    let d =
      tryWith'
        "Supervisor.tryFinally"
        (fun () -> Deferred.map (f ()) (fun x -> Result.Result x))
        (fun ex -> Deferred.create (Result.Exception ex))
        AfterDetermined.Log
    Deferred.bind d (fun result ->
      Deferred.map (finalizer ()) (fun () ->
        match result with
        | Result.Result x -> x
        | Result.Exception ex -> systemRaise ex
      )
    )
