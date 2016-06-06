namespace Wj.Async

open System

exception SupervisorChildException of supervisorNames : string list * innerException : exn

module Supervisor =
  let [<Literal>] private cannotAddHandlerToRoot =
    "Handlers cannot be registered on the root supervisor."

  // ISupervisor functions
  let parent (t : ISupervisor) = t.Parent
  let name (t : ISupervisor) = t.Name
  let detach (t : ISupervisor) = t.Detach()
  let sendException (t : ISupervisor) ex = t.SendException(ex)
  let uponException (t : ISupervisor) (handler : exn -> unit) = t.UponException(handler)
  let uponException' (t : ISupervisor) (supervisedHandler : exn SupervisedCallback) =
    t.UponException(supervisedHandler)
  let run (t : ISupervisor) f = t.Run(f)

  module Child =
    type T =
      { name : string;
        mutable parent : ISupervisor option;
        mutable handlers : exn SupervisedCallback list; }

      interface ISupervisor with
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
        parent = ThreadShared.tryCurrentSupervisor ();
        handlers = []; }
      :> ISupervisor

  module Root =
    type T =
      | Root

      interface ISupervisor with
        member t.Parent = None

        member t.Name = "Root"

        member t.SendException(ex) = raise ex

        member t.Detach() = ()

        member t.UponException(handler : exn -> unit) : unit =
          invalidOp cannotAddHandlerToRoot

        member t.UponException(supervisedHandler : exn SupervisedCallback) : unit =
          invalidOp cannotAddHandlerToRoot

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

  let handleAfterDetermined afterDetermined ex =
    match afterDetermined with
    | AfterDetermined.Raise -> raise ex
    | AfterDetermined.Log -> stderr.WriteLine(sprintf "Unhandled exception after tryWith:\n%s" (string ex))
    | AfterDetermined.Ignore -> ()

  let tryWith' name (f : unit -> _ IDeferred) (handler : exn -> _ IDeferred) afterDetermined =
    let t = createNamed name
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
        let reader = Deferred.createNode ()
        let mutable writer = Some reader
        let write v d =
          Deferred.link v d
          writer <- None
        uponException t (fun ex ->
          match writer with
          | Some v -> write v (handler ex)
          | None -> handleAfterDetermined afterDetermined ex
        )
        Deferred.upon' d (root, (fun x ->
          match writer with
          | Some v -> write v d
          | None -> ()
        ))
        reader :> _ IDeferred
    | Result.Failure ex ->
      startHandlingAfterDetermined ()
      handler ex

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
        | Result.Failure ex -> raise ex
      )
    )
