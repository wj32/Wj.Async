namespace Wj.Async

open System;
open System.Threading.Tasks;

module Deferred =
  let [<Literal>] private DeferredNotDetermined = "The deferred result is not yet determined."
  let [<Literal>] private VarAlreadySet = "The variable is already set."
  let [<Literal>] private NodeAlreadyLinked = "The node has already been joined."

  // IDeferred functions
  let upon (t : _ IDeferred) (f : _ -> unit) = t.Upon(f)
  let upon' (t : _ IDeferred) (supervisedCallback : _ SupervisedCallback) = t.Upon(supervisedCallback)
  let register (t : _ IDeferred) (f : _ -> unit) = t.Register(f)
  let register' (t : _ IDeferred) (supervisedCallback : _ SupervisedCallback) = t.Register(supervisedCallback)
  let moveFrom (t : _ IDeferred) from = t.MoveFrom(from)
  let get (t : _ IDeferred) = t.Get()
  let tryGet (t : _ IDeferred) = t.TryGet()
  let isDetermined (t : _ IDeferred) = t.IsDetermined
  // IVar functions
  let set (t : _ IVar) x = t.Set(x)
  let trySet (t : _ IVar) x = t.TrySet(x)
  // INode functions
  let isLinked (t : _ INode) = t.IsLinked
  let link (t : _ INode) d = t.Link(d)
  let tryLink (t : _ INode) d = t.TryLink(d)

  let enqueue supervisor f x =
    let dispatcher = ThreadShared.currentDispatcher ()
    dispatcher.Enqueue((supervisor, fun () -> f x))

  module private Var =
    [<ReferenceEquality>]
    type 'a Pending =
      { dispatcher : IDispatcher;
        callbacks : 'a SupervisedCallback RegistrationList.T; }

    [<ReferenceEquality>]
    type 'a State =
      | Pending of 'a Pending
      | Done of 'a

    [<ReferenceEquality>]
    type 'a T =
      {mutable state : 'a State}

      interface 'a IDeferred with
        member t.Upon(f) = (t :> _ IDeferred).Upon((ThreadShared.currentSupervisor (), f))

        member t.Upon((supervisor, f) as supervisedCallback) =
          match t.state with
          | Pending pending -> RegistrationList.add pending.callbacks supervisedCallback
          | Done x -> enqueue supervisor f x

        member t.Register(f) = (t :> _ IDeferred).Register((ThreadShared.currentSupervisor (), f))

        member t.Register((supervisor, f) as supervisedCallback) =
          match t.state with
          | Pending pending -> RegistrationList.register pending.callbacks supervisedCallback
          | Done x -> enqueue supervisor f x; Registration.empty

        member t.MoveFrom(from) =
          match t.state with
          | Pending pending -> RegistrationList.moveFrom pending.callbacks from
          | Done x ->
            let dispatcher = ThreadShared.currentDispatcher ()
            RegistrationList.toList from |> List.iter (fun (supervisor, f) ->
              dispatcher.Enqueue((supervisor, fun () -> f x))
            )

        member t.Get() =
          match t.state with
          | Pending _ -> invalidOp DeferredNotDetermined
          | Done x -> x

        member t.TryGet() =
          match t.state with
          | Pending _ -> None
          | Done x -> Some x

        member t.IsDetermined =
          match t.state with
          | Pending _ -> false
          | Done _ -> true

      interface 'a IVar with
        member t.Set(x) =
          if not ((t :> 'a IVar).TrySet(x)) then
            invalidOp VarAlreadySet

        member t.TrySet(x) =
          match t.state with
          | Pending pending ->
            t.state <- Done x
            RegistrationList.toList pending.callbacks |> List.iter (fun (supervisor, f) ->
              pending.dispatcher.Enqueue((supervisor, fun () -> f x))
            )
            true
          | Done _ -> false

    let createPending dispatcher =
      { state = Pending
          { dispatcher = ThreadShared.currentDispatcher ();
            callbacks = RegistrationList.create () }; }

    let createDone x = {state = Done x}

  module private Never =
    [<ReferenceEquality>]
    type 'a T =
      | Never

      interface 'a IDeferred with
        member t.Upon(f : 'a -> unit) = ()

        member t.Upon(supervisedCallback : 'a SupervisedCallback) = ()

        member t.Register(f : 'a -> unit) = Registration.empty

        member t.Register(supervisedCallback : 'a SupervisedCallback) = Registration.empty

        member t.MoveFrom(from) = RegistrationList.clear from

        member t.Get() = invalidOp DeferredNotDetermined

        member t.TryGet() = None

        member t.IsDetermined = false

    let create () = Never

  module private Node =
    [<ReferenceEquality>]
    type 'a State =
      /// The node has no parent.
      | Unlinked of 'a SupervisedCallback RegistrationList.T
      /// The node has a parent IDeferred.
      /// Due to FindRoot, the parent will always be an Unlinked or not a Node.T at all (but never a
      /// Linked).
      | Linked of parent : 'a IDeferred

    [<ReferenceEquality>]
    type 'a T =
      { mutable state : 'a State; }

      static member private FindRoot (d : 'a IDeferred) =
        let rec findRoot (d : 'a IDeferred) =
          match d with
          | :? ('a T) as t ->
            match t.state with
            | Unlinked _ -> d
            | Linked parent -> findRoot parent
          | _ -> d
        let rec updateParent (d : 'a IDeferred) root =
          match d with
          | :? ('a T) as t ->
            match t.state with
            | Unlinked _ -> ()
            | Linked parent ->
              t.state <- Linked root
              updateParent parent root
          | _ -> ()
        match d with
        | :? ('a T) as t ->
          match t.state with
          | Unlinked _ -> d
          | Linked parent ->
            let root = findRoot parent
            updateParent t root
            root
        | _ -> d

      interface 'a IDeferred with
        member t.Upon(f) = (t :> _ IDeferred).Upon((ThreadShared.currentSupervisor (), f))

        member t.Upon(supervisedCallback) =
          match t.state with
          | Unlinked callbacks -> RegistrationList.add callbacks supervisedCallback
          | Linked parent -> upon' (T<'a>.FindRoot(parent)) supervisedCallback

        member t.Register(f) = (t :> _ IDeferred).Register((ThreadShared.currentSupervisor (), f))

        member t.Register(supervisedCallback) =
          match t.state with
          | Unlinked callbacks -> RegistrationList.register callbacks supervisedCallback
          | Linked parent -> register' (T<'a>.FindRoot(parent)) supervisedCallback

        member t.MoveFrom(from) =
          match t.state with
          | Unlinked callbacks -> RegistrationList.moveFrom callbacks from
          | Linked parent -> moveFrom (T<'a>.FindRoot(parent)) from

        member t.Get() =
          match t.state with
          | Unlinked _ -> invalidOp DeferredNotDetermined
          | Linked parent -> get (T<'a>.FindRoot(parent))

        member t.TryGet() =
          match t.state with
          | Unlinked _ -> None
          | Linked parent -> tryGet (T<'a>.FindRoot(parent))

        member t.IsDetermined =
          match t.state with
          | Unlinked _ -> false
          | Linked parent -> isDetermined (T<'a>.FindRoot(parent))

      interface 'a INode with
        member t.IsLinked =
          match t.state with
          | Unlinked _ -> false
          | Linked _ -> true

        member t.Link(parent) =
          if not ((t :> _ INode).TryLink(parent)) then
            invalidOp NodeAlreadyLinked

        member t.TryLink(parent) =
          match t.state with
          | Unlinked callbacks ->
            // moveFrom parent callbacks does the same thing, but this should be slightly faster.
            let root = T<'a>.FindRoot(parent)
            moveFrom root callbacks
            t.state <- Linked root
            true
          | Linked _ -> false

    let create () = {state = Unlinked (RegistrationList.create ())}

  let create x = Var.createDone x :> _ IDeferred

  let createVar () = Var.createPending (ThreadShared.currentDispatcher ()) :> _ IVar

  let createNode () = Node.create () :> _ INode

  let unit = create ()

  let never () = Never.create () :> _ IDeferred

  let ``return`` x = create x

  let bind t (f : _ -> _ IDeferred) =
    let v = createNode ()
    upon t (fun x -> link v (f x))
    v :> _ IDeferred

  let map t f =
    let v = createVar ()
    upon t (fun x -> set v (f x))
    v :> _ IDeferred

  let join t = bind t id

  let forget t = map t ignore

  let all (ts : _ IDeferred list) =
    let count = List.length ts
    let xs = Array.zeroCreate count
    let mutable completed = 0
    let v = createVar ()
    ts |> List.iteri (fun i t ->
      upon t (fun x ->
        xs.[i] <- x
        completed <- completed + 1
        if completed = count then
          set v (List.ofArray xs)
      )
    )
    v :> _ IDeferred

  let allForget (ts : unit IDeferred list) =
    let count = List.length ts
    let mutable completed = 0
    let v = createVar ()
    ts |> List.iter (fun t ->
      upon t (fun x ->
        completed <- completed + 1
        if completed = count then
          set v ()
      )
    )
    v :> _ IDeferred

  let both (t1 : 'a IDeferred) (t2 : 'b IDeferred) =
    let v = createVar ()
    upon t1 (fun x1 -> upon t2 (fun x2 -> set v (x1, x2)))
    v :> _ IDeferred

  let allUnit ts = allForget ts

  let any (ts : _ IDeferred list) =
    let v = createVar ()
    ts |> List.iter (fun t -> upon t (trySet v >> ignore))
    v :> _ IDeferred

  let anyi (ts : _ IDeferred list) =
    let v = createVar ()
    ts |> List.iteri (fun i t -> upon t (fun x -> trySet v (x, i) |> ignore))
    v :> _ IDeferred

  let anyUnit (ts : _ IDeferred list) = map (any ts) ignore

  let dontWaitFor (t : unit IDeferred) = ()

  let ofAsync a =
    let v = createVar ()
    let a = async {
      let! x = a
      set v x
    }
    (a, v :> _ IDeferred)

  let ofTask (task : 'a Task) =
    let v = createVar ()
    task.ContinueWith(Action<Task<'a>>(fun _ -> v.Set(task.Result))) |> ignore
    v :> _ IDeferred

  let ofTaskUnit (task : Task) =
    let v = createVar ()
    task.ContinueWith(Action<Task>(fun _ -> v.Set(()))) |> ignore
    v :> _ IDeferred

  module Infix =
    let (>>=) t f = bind t f

    let (>>|) t f = map t f

    let (>>>) t f = upon t f
