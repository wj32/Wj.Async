namespace Wj.Async

open System
open System.Threading

module Cancellation =
  open Deferred.Infix

  let [<Literal>] CancellationNotRequested = "Cancellation has not yet been requested."

  type T = unit IDeferred

  module CancellationTokenWrapper =
    [<ReferenceEquality>]
    type 'a State =
      | Unregistered of token : CancellationToken
      | Registered of
        token : CancellationToken *
        registration : CancellationTokenRegistration *
        callbacks : 'a SupervisedCallback RegistrationList.T
      | Cancelled

    // We cannot implement the unit IDeferred interface directly. Here the IDeferred function
    // Get : unit -> 'a would become Get : unit -> unit, but F# converts the return type to void and
    // prevents us from implementing the function. We work around this by keeping the 'a type
    // parameter (in Generic) and specifying 'a = unit later on (in T).

    [<ReferenceEquality>]
    type 'a Generic =
      {mutable state : 'a State}

      member inline t.Value = Unchecked.defaultof<'a>

      member inline t.Enqueue((supervisor, f)) =
        Dispatcher.enqueue (Supervisor.dispatcher supervisor) (supervisor, fun () -> f t.Value)

      member t.CancellationCallback(param : obj) =
        let supervisor = param :?> ISupervisor
        Dispatcher.enqueue (Supervisor.dispatcher supervisor) (supervisor, fun () ->
          let execute list =
            for supervisedCallback in RegistrationList.toList list do
              t.Enqueue(supervisedCallback)
          match t.state with
          | Unregistered _ -> ()
          | Registered (_, _, callbacks) -> t.state <- Cancelled; execute callbacks
          | Cancelled -> ()
        )

      member t.OnEmptyCallback() =
        match t.state with
        | Unregistered _ -> failwith "Unexpected 'Unregistered' state"
        | Registered (token, registration, callbacks) ->
          assert (RegistrationList.isEmpty callbacks)
          registration.Dispose()
          t.state <- Unregistered token
        | Cancelled -> ()

      member inline t.RegisterInternal(register, empty, execute) =
        match t.state with
        | Unregistered token ->
          if token.IsCancellationRequested then
            t.state <- Cancelled
            execute ()
            empty ()
          else
            let callbacks = RegistrationList.create ()
            let registration =
              token.Register(new Action<obj>(t.CancellationCallback), Supervisor.current ())
            t.state <- Registered (token, registration, callbacks)
            let result = register callbacks
            RegistrationList.addOnEmptyCallback callbacks t.OnEmptyCallback
            result
        | Registered (_, _, callbacks) -> register callbacks
        | Cancelled -> execute (); empty ()

      interface 'a IDeferred with
        member t.Upon(f) = (t :> _ IDeferred).Upon((Supervisor.current (), f))

        member t.Upon(supervisedCallback) =
          t.RegisterInternal(
            (fun callbacks -> RegistrationList.add callbacks supervisedCallback),
            id,
            (fun () -> t.Enqueue(supervisedCallback))
          )

        member t.Register(f : _ -> unit) = (t :> _ IDeferred).Register((Supervisor.current (), f))

        member t.Register((supervisor, f) as supervisedCallback) =
          t.RegisterInternal(
            (fun callbacks -> RegistrationList.register callbacks supervisedCallback),
            (fun () -> Registration.empty),
            (fun () -> t.Enqueue(supervisedCallback))
          )

        member t.MoveFrom(from) =
          if not (RegistrationList.isEmpty from) then
            t.RegisterInternal(
              (fun callbacks -> RegistrationList.moveFrom callbacks from),
              id,
              (fun () ->
                for supervisedCallback in RegistrationList.toList from do
                  t.Enqueue(supervisedCallback)
              )
            )

        member t.IsDetermined =
          match t.state with
          | Unregistered token -> token.IsCancellationRequested
          | Registered _ -> false
          | Cancelled -> true

        member t.Get() =
          match t.state with
          | Unregistered token ->
            if token.IsCancellationRequested then t.Value else invalidOp CancellationNotRequested
          | Registered _ -> invalidOp CancellationNotRequested
          | Cancelled -> t.Value

        member t.TryGet() =
          match t.state with
          | Unregistered token -> if token.IsCancellationRequested then Some t.Value else None
          | Registered _ -> None
          | Cancelled -> Some t.Value

    type T = unit Generic

    let inline create token : T = {state = Unregistered token}

  module Source =
    type T = unit IVar

    let inline create () : T = Deferred.createVar ()

    let inline set source = Deferred.trySet source () |> ignore

  let now = Deferred.unit

  let never : T = Deferred.never ()

  let inline isSet (t : T) = Deferred.isDetermined t

  let inline raiseIfSet (t : T) = if isSet t then raise (new OperationCanceledException())

  let run (f : unit -> _ IDeferred) =
    let supervisor = Supervisor.current ()
    Supervisor.tryWith (fun () -> f () >>| Some) (fun ex ->
      match ex with
      | :? OperationCanceledException -> Deferred.value None
      | _ -> Supervisor.sendException supervisor ex; Deferred.never ()
    ) Supervisor.AfterDetermined.Ignore

  let inline ofSource (source : Source.T) = source :> T

  let ofToken token = CancellationTokenWrapper.create token :> T

  let toToken t =
    let source = new CancellationTokenSource()
    t >>> (source.Cancel : unit -> unit)
    source.Token

  module Option =
    let inline join t = match t with Some t -> t | None -> never

    let inline isSet (t : T option) = match t with Some t -> isSet t | None -> false

    let inline raiseIfSet (t : T option) = match t with Some t -> raiseIfSet t | None -> ()

    let inline ofSource source = Some (ofSource source)

    let inline ofToken token = Some (ofToken token)

    let inline toToken (t : T option) =
      match t with Some t -> toToken t | None -> CancellationToken.None
