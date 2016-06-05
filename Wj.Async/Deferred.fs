﻿namespace Wj.Async

open System;
open System.Threading.Tasks;

module Deferred =
  let [<Literal>] private DeferredNotDetermined = "The deferred result is not yet determined."
  let [<Literal>] private VarAlreadySet = "The variable is already set."
  let [<Literal>] private NodeAlreadyLinked = "The node has already been joined."

  // IDeferred functions
  let upon (t : _ IDeferred) f = t.Upon(f)
  let get (t : _ IDeferred) = t.Get()
  let tryGet (t : _ IDeferred) = t.TryGet()
  let isDetermined (t : _ IDeferred) = t.IsDetermined
  // IVar functions
  let set (t : _ IVar) x = t.Set(x)
  let trySet (t : _ IVar) x = t.TrySet(x)
  // INode functions
  let link (t : _ INode) d = t.Link(d)

  module private Var =
    [<ReferenceEqualityAttribute>]
    type 'a Pending =
      { dispatcher : IDispatcher;
        mutable callbacks : ('a -> unit) list; }

    [<ReferenceEqualityAttribute>]
    type 'a State =
      | Pending of 'a Pending
      | Done of 'a

    [<ReferenceEqualityAttribute>]
    type 'a T =
      { mutable state : 'a State; }

      interface 'a IDeferred with
        member t.Upon f =
          match t.state with
          | Pending pending -> pending.callbacks <- f :: pending.callbacks
          | Done x -> (ThreadDispatcher.current ()).Enqueue(fun () -> f x)

        member t.Get () =
          match t.state with
          | Pending _ -> invalidOp DeferredNotDetermined
          | Done x -> x

        member t.TryGet () =
          match t.state with
          | Pending _ -> None
          | Done x -> Some x

        member t.IsDetermined =
          match t.state with
          | Pending _ -> false
          | Done _ -> true

      interface 'a IVar with
        member t.Set x =
          if not ((t :> 'a IVar).TrySet(x)) then
            invalidOp VarAlreadySet

        member t.TrySet x =
          match t.state with
          | Pending pending ->
            t.state <- Done x
            let callbacks = List.rev pending.callbacks
            pending.dispatcher.Enqueue(fun () -> callbacks |> List.iter (fun f -> f x))
            true
          | Done _ -> false

    let createPending dispatcher =
      { state = Pending
          { dispatcher = ThreadDispatcher.current ();
            callbacks = []; }; }

    let createDone x = { state = Done x; }

  module private Never =
    [<ReferenceEqualityAttribute>]
    type 'a T =
      | Never

      interface 'a IDeferred with
        member t.Upon f = ()

        member t.Get () = invalidOp DeferredNotDetermined

        member t.TryGet () = None

        member t.IsDetermined = false

    let create () = Never

  module private Mapping =
    [<ReferenceEqualityAttribute>]
    type State<'a, 'b> =
      | Pending of parent:'a IDeferred * mapping:('a -> 'b)
      | Done of 'b

    [<ReferenceEqualityAttribute>]
    type T<'a, 'b> =
      { mutable state : State<'a, 'b>; }

      interface 'b IDeferred with
        member t.Upon f =
          match t.state with
          | Pending (parent, mapping) ->
            upon parent (fun x ->
              let y =
                match t.state with
                | Pending _ ->
                  let y = mapping x
                  t.state <- Done y
                  y
                | Done y -> y
              f y
            )
          | Done y -> (ThreadDispatcher.current ()).Enqueue(fun () -> f y)

        member t.Get () =
          match t.state with
          | Pending _ -> invalidOp DeferredNotDetermined
          | Done y -> y

        member t.TryGet () =
          match t.state with
          | Pending _ -> None
          | Done y -> Some y

        member t.IsDetermined =
          match t.state with
          | Pending _ -> false
          | Done y -> true

    let create parent mapping = { state = Pending (parent, mapping); }

  module private Node =
    [<ReferenceEqualityAttribute>]
    type 'a Unlinked =
      { mutable callbacks : ('a -> unit) list; }

    [<ReferenceEqualityAttribute>]
    type 'a State =
      /// The node has no parent.
      | Unlinked of 'a Unlinked
      /// The node has a parent IDeferred.
      /// Due to FindRoot, the parent will always be an Unlinked or not a Node.T at all (but never a
      /// Linked).
      | Linked of parent:'a IDeferred

    [<ReferenceEqualityAttribute>]
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
        member t.Upon f =
          match t.state with
          | Unlinked unlinked -> unlinked.callbacks <- f :: unlinked.callbacks
          | Linked parent -> upon (T<'a>.FindRoot(parent)) f

        member t.Get () =
          match t.state with
          | Unlinked _ -> invalidOp DeferredNotDetermined
          | Linked parent -> get (T<'a>.FindRoot(parent))

        member t.TryGet () =
          match t.state with
          | Unlinked _ -> invalidOp DeferredNotDetermined
          | Linked parent -> tryGet (T<'a>.FindRoot(parent))

        member t.IsDetermined =
          match t.state with
          | Unlinked _ -> false
          | Linked parent -> isDetermined (T<'a>.FindRoot(parent))

      interface INode<'a> with
        member t.Link parent =
          match t.state with
          | Unlinked unlinked ->
            let root = T<'a>.FindRoot(parent)
            unlinked.callbacks |> List.iter (upon root)
            t.state <- Linked root
          | Linked _ -> invalidOp NodeAlreadyLinked

    let create () =
      { state = Unlinked
          { callbacks = []; }; }

  let create x = Var.createDone x :> _ IDeferred

  let createVar () = Var.createPending (ThreadDispatcher.current ()) :> _ IVar

  let createNode () = Node.create () :> _ INode

  let unit = create ()

  let never () = Never.create () :> _ IDeferred

  let lift x = create x

  let bind t (f : _ -> _ IDeferred) =
    let v = createNode ()
    upon t (fun x -> link v (f x))
    v :> _ IDeferred

  let map t f = Mapping.create t f :> _ IDeferred

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

  let allForget (ts : unit IDeferred list) = map (all ts) ignore

  let both (t1 : 'a IDeferred) (t2 : 'b IDeferred) =
    let mutable x1 = Unchecked.defaultof<'a>
    let mutable x2 = Unchecked.defaultof<'b>
    let mutable completed = 0
    let v = createVar ()
    let increment () =
      completed <- completed + 1
      if completed = 2 then
        set v (x1, x2)
    upon t1 (fun x ->
      x1 <- x
      increment ()
    )
    upon t2 (fun x ->
      x2 <- x
      increment ()
    )
    v :> _ IDeferred

  let allUnit ts = allForget ts

  let any (ts : _ IDeferred list) =
    let v = createVar ()
    ts |> List.iter (fun t -> upon t (trySet v >> ignore))
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
