namespace Wj.Async

open System;
open System.Threading.Tasks;

module Deferred =
  module Var =
    [<ReferenceEqualityAttribute>]
    type 'a Pending =
      { dispatcher : IDispatcher;
        mutable callbacks : ('a -> unit) list; }

    [<ReferenceEqualityAttribute>]
    type 'a State =
      | Pending of 'a Pending
      | Done of 'a
      | Never

    [<ReferenceEqualityAttribute>]
    type 'a T =
      { mutable state : 'a State; }

      interface IDeferred<'a> with
        member t.Upon f =
          match t.state with
          | Pending pending -> pending.callbacks <- f :: pending.callbacks
          | Done x -> (ThreadDispatcher.current ()).Enqueue(fun () -> f x)
          | Never -> ()

        member t.Get () =
          match t.state with
          | Pending _ | Never -> invalidOp "The deferred result is not yet available."
          | Done x -> x

        member t.TryGet () =
          match t.state with
          | Pending _ | Never -> None
          | Done x -> Some x

        member t.IsDetermined =
          match t.state with
          | Pending _ | Never -> false
          | Done _ -> true

      interface IVar<'a> with
        member t.Set x =
          if not ((t :> IVar<'a>).TrySet(x)) then
            invalidOp "The variable is already set."

        member t.TrySet x =
          match t.state with
          | Pending pending ->
            let callbacks = List.rev pending.callbacks
            t.state <- Done x
            pending.dispatcher.Enqueue(fun () -> callbacks |> List.iter (fun f -> f x))
            true
          | Done _ -> false
          | Never -> invalidOp "This variable cannot be set."

    let createPending dispatcher =
      { state = Pending
          { dispatcher = ThreadDispatcher.current ();
            callbacks = []; }; }

    let createDone x = { state = Done x; }

    let createNever () = { state = Never; }

  module Mapped =
    [<ReferenceEqualityAttribute>]
    type T<'a, 'b> =
      { parent : 'a IDeferred;
        mapping : 'a -> 'b; }

      interface IDeferred<'b> with
        member t.Upon f = t.parent.Upon(fun x -> f (t.mapping x))

        member t.Get () = t.mapping (t.parent.Get())

        member t.TryGet () = Option.map t.mapping (t.parent.TryGet())

        member t.IsDetermined = t.parent.IsDetermined

    let create parent mapping =
      { parent = parent;
        mapping = mapping; }

  let upon (t : _ IDeferred) f = t.Upon(f)
  let get (t : _ IDeferred) = t.Get()
  let tryGet (t : _ IDeferred) = t.TryGet()
  let isDetermined (t : _ IDeferred) = t.IsDetermined
  let set (t : _ IVar) x = t.Set(x)
  let trySet (t : _ IVar) x = t.TrySet(x)

  let value x = Var.createDone x :> _ IDeferred

  let createVar () = Var.createPending (ThreadDispatcher.current ()) :> _ IVar

  let unit = value ()

  let never () = Var.createNever () :> _ IDeferred

  let lift x = value x

  let bind (t : _ IDeferred) (f : _ -> _ IDeferred) =
    // TODO: For infinite deferred loops, space usage grows linearly due to a long backwards chain
    // of these temporary vars.
    let v = createVar ()
    upon t (fun x -> upon (f x) (fun y -> set v y))
    v :> _ IDeferred

  let map t f = Mapped.create t f :> _ IDeferred

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
