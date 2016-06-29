namespace Wj.Async

open System
open System.Threading

module CancellationSignal =
  open Deferred.Infix

  let [<Literal>] CancellationNotRequested = "Cancellation has not yet been requested."

  type T = unit IDeferred
  type Source = unit IVar

  module Wrapper =
    [<ReferenceEquality>]
    type 'a State =
      | Unregistered of token : CancellationToken
      | Registered of callbacks : 'a SupervisedCallback RegistrationList.T
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
          let execute list = RegistrationList.toList list |> List.iter t.Enqueue
          match t.state with
          | Unregistered _ -> failwith "Unexpected 'Unregistered' state"
          | Registered callbacks -> t.state <- Cancelled; execute callbacks
          | Cancelled -> () // Currently this should never happen, but it is still OK.
        )

      member inline t.RegisterInternal(register, empty, execute) =
        match t.state with
        | Unregistered token ->
          if token.IsCancellationRequested then
            t.state <- Cancelled
            execute ()
            empty ()
          else
            let callbacks = RegistrationList.create ()
            token.Register(new Action<obj>(t.CancellationCallback), Supervisor.current ()) |> ignore
            t.state <- Registered callbacks
            register callbacks
        | Registered callbacks -> register callbacks
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
          t.RegisterInternal(
            (fun callbacks -> RegistrationList.moveFrom callbacks from),
            id,
            (fun () -> RegistrationList.toList from |> List.iter t.Enqueue)
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

  let now = Deferred.unit

  let never : T = Deferred.never ()

  let inline isSet (t : T) = Deferred.isDetermined t

  let raiseIfSet (t : T) = if isSet t then raise (new OperationCanceledException())

  let ofToken token = Wrapper.create token :> T

  let toToken t =
    let source = new CancellationTokenSource()
    t >>> (source.Cancel : unit -> unit)
    source.Token

  let inline create () : Source = Deferred.createVar ()

  let inline ofSource (source : Source) = source :> T

  let inline set source = Deferred.trySet source () |> ignore
