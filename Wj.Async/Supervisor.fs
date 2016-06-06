namespace Wj.Async

open System

exception SupervisorChildException of string list * exn

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
        mutable handlers : (ISupervisor * (exn -> unit)) list; }

      interface ISupervisor with
        member t.Parent = t.parent

        member t.Name = t.name

        member t.Raise(ex) =
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
          let result = Result.tryWith f
          ThreadShared.popSupervisor t
          match result with
          | Result.Failure ex -> (t :> ISupervisor).Raise(ex)
          | Result.Success _ -> ()
          result

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

        member t.Run(f) = Result.tryWith f

  let root = Root.Root :> ISupervisor

  let current () = ThreadShared.currentSupervisor ()

  let create () = Child.create "Unnamed supervisor"

  let createNamed name = Child.create name

  let supervise (f : unit -> _ IDeferred) observer =
    let t = createNamed "Supervisor.supervise"
    uponException t observer
    let result = run t f
    match result with
    | Result.Success d -> d
    | Result.Failure _ -> Deferred.never ()

  module AfterDetermined =
    type T = Raise | Log | Ignore

  let raiseAfterDetermined afterDetermined ex =
    match afterDetermined with
    | AfterDetermined.Raise -> systemRaise ex
    | AfterDetermined.Log -> stderr.WriteLine(sprintf "Unhandled exception after tryWith:\n%s" (string ex))
    | AfterDetermined.Ignore -> ()

  let tryWith' name (f : unit -> _ IDeferred) (handler : exn -> _ IDeferred) afterDetermined =
    let v = Deferred.createNode ()
    let mutable writer = Some v
    let t = createNamed name
    detach t
    uponException t (fun ex ->
      match writer with
      | Some v ->
        Deferred.link v (handler ex)
        writer <- None
      | None -> raiseAfterDetermined afterDetermined ex
    )
    let result = run t f
    match result with
    | Result.Success d ->
      Deferred.upon d (fun x ->
        match writer with
        | Some v ->
          Deferred.link v d
          writer <- None
        | None -> ()
      )
    | Result.Failure _ -> ()
    v :> _ IDeferred

  let tryWith f handler afterDetermined =
    tryWith' "Supervisor.tryWith" f handler afterDetermined

  let tryFinally (f : unit -> _ IDeferred) (finalizer : unit -> _ IDeferred) =
    let d =
      tryWith'
        "Supervisor.tryFinally"
        (fun () -> Deferred.map (f ()) (fun x -> Result.Success x))
        (fun ex -> Deferred.create (Result.Failure ex))
        AfterDetermined.Log
    Deferred.bind d (fun result ->
      Deferred.map (finalizer ()) (fun () ->
        match result with
        | Result.Success x -> x
        | Result.Failure ex -> systemRaise ex
      )
    )
