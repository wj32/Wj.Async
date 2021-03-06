﻿namespace Wj.Async

open System
open System.Threading
open System.Threading.Tasks
open Wj.Async.Internal

module Deferred =
  let [<Literal>] DeferredNotDetermined = "The deferred result is not yet determined."
  let [<Literal>] VarAlreadySetOrLinked = "The variable is already set or linked."

  // IDeferred functions
  let inline upon (t : _ IDeferred) (f : _ -> unit) = t.Upon(f)
  let inline upon' (t : _ IDeferred) (supervisedCallback : _ SupervisedCallback) = t.Upon(supervisedCallback)
  let inline register (t : _ IDeferred) (f : _ -> unit) = t.Register(f)
  let inline register' (t : _ IDeferred) (supervisedCallback : _ SupervisedCallback) = t.Register(supervisedCallback)
  let inline moveFrom (t : _ IDeferred) from = t.MoveFrom(from)
  let inline isDetermined (t : _ IDeferred) = t.IsDetermined
  let inline get (t : _ IDeferred) = t.Get()
  let inline tryGet (t : _ IDeferred) = t.TryGet()
  // IVar functions
  let inline set (t : _ IVar) x = t.Set(x)
  let inline trySet (t : _ IVar) x = t.TrySet(x)
  let inline link (t : _ IVar) d = t.Link(d)
  let inline tryLink (t : _ IVar) d = t.TryLink(d)

  let inline enqueue (supervisor : ISupervisor) f x =
    supervisor.Dispatcher.Enqueue(supervisor, fun () -> f x)

  module Never =
    [<ReferenceEquality>]
    type 'a T =
      | Never

      interface 'a IDeferred with
        member t.Upon(f : 'a -> unit) = ()

        member t.Upon(supervisedCallback : 'a SupervisedCallback) = ()

        member t.Register(f : 'a -> unit) = Registration.empty

        member t.Register(supervisedCallback : 'a SupervisedCallback) = Registration.empty

        member t.MoveFrom(from) = RegistrationList.clear from

        member t.IsDetermined = false

        member t.Get() = invalidOp DeferredNotDetermined

        member t.TryGet() = None

    let inline never () = Never

  module Var =
    [<ReferenceEquality>]
    type 'a State =
      /// The Var has no value, is not linked, and has no callbacks.
      | Pending
      /// The Var has no value, is not linked, and has one callback (which cannot be removed).
      | PendingOne of callback : 'a SupervisedCallback
      /// The Var has no value, is not linked, and may have many callbacks.
      | PendingMany of callbacks : 'a SupervisedCallback RegistrationList.T
      /// The Var is set to a value.
      | Value of value : 'a
      /// The Var is linked to a parent IDeferred.
      /// Due to FindRoot, the parent will always be a Pending*, Value or not a Var.T at all (but
      /// never a Linked).
      | Linked of parent : 'a IDeferred

    [<ReferenceEquality>]
    type 'a T =
      {mutable state : 'a State}

      static member FindRoot (d : 'a IDeferred) =
        let rec findRoot original (d : 'a IDeferred) =
          match d with
          | :? ('a T) as t ->
            match t.state with
            | Linked parent ->
              if obj.ReferenceEquals(parent, original) then
                // Cycle detected!
                original
              else
                findRoot original parent
            | _ -> d
          | _ -> d
        let rec updateParent (d : 'a IDeferred) root =
          match d with
          | :? ('a T) as t ->
            match t.state with
            | Linked parent ->
              t.state <- Linked root
              updateParent parent root
            | _ -> ()
          | _ -> ()
        match d with
        | :? ('a T) as t ->
          match t.state with
          | Linked parent ->
            let root = findRoot d parent
            updateParent t root
            root
          | _ -> d
        | _ -> d

      interface 'a IDeferred with
        member t.Upon(f) = (t :> _ IDeferred).Upon((ThreadShared.currentSupervisor (), f))

        member t.Upon((supervisor, f) as supervisedCallback) =
          match t.state with
          | Pending -> t.state <- PendingOne supervisedCallback
          | PendingOne existingCallback ->
            let callbacks = RegistrationList.create ()
            t.state <- PendingMany callbacks
            RegistrationList.add callbacks existingCallback
            RegistrationList.add callbacks supervisedCallback
          | PendingMany callbacks -> RegistrationList.add callbacks supervisedCallback
          | Value x -> enqueue supervisor f x
          | Linked parent -> upon' (T<'a>.FindRoot(parent)) supervisedCallback

        member t.Register(f) = (t :> _ IDeferred).Register((ThreadShared.currentSupervisor (), f))

        member t.Register((supervisor, f) as supervisedCallback) =
          let inline convertAndRegister action =
            let callbacks = RegistrationList.create ()
            t.state <- PendingMany callbacks
            action callbacks
            RegistrationList.register callbacks supervisedCallback
          match t.state with
          | Pending -> convertAndRegister ignore
          | PendingOne callback ->
            convertAndRegister (fun callbacks -> RegistrationList.add callbacks callback)
          | PendingMany callbacks -> RegistrationList.register callbacks supervisedCallback
          | Value x -> enqueue supervisor f x; Registration.empty
          | Linked parent -> register' (T<'a>.FindRoot(parent)) supervisedCallback

        member t.MoveFrom(from) =
          let inline convertAndMoveFrom action =
            let callbacks = RegistrationList.create ()
            t.state <- PendingMany callbacks
            action callbacks
            RegistrationList.moveFrom callbacks from
          match t.state with
          | Pending -> convertAndMoveFrom ignore
          | PendingOne callback ->
            convertAndMoveFrom (fun callbacks -> RegistrationList.add callbacks callback)
          | PendingMany callbacks -> RegistrationList.moveFrom callbacks from
          | Value x -> for (supervisor, f) in RegistrationList.toList from do enqueue supervisor f x
          | Linked parent -> moveFrom (T<'a>.FindRoot(parent)) from

        member t.IsDetermined =
          match t.state with
          | Pending | PendingOne _ | PendingMany _ -> false
          | Value _ -> true
          | Linked parent -> isDetermined (T<'a>.FindRoot(parent))

        member t.Get() =
          match t.state with
          | Pending | PendingOne _ | PendingMany _ -> invalidOp DeferredNotDetermined
          | Value x -> x
          | Linked parent -> get (T<'a>.FindRoot(parent))

        member t.TryGet() =
          match t.state with
          | Pending | PendingOne _ | PendingMany _ -> None
          | Value x -> Some x
          | Linked parent -> tryGet (T<'a>.FindRoot(parent))

      interface 'a IVar with
        member t.Set(x) =
          if not ((t :> 'a IVar).TrySet(x)) then
            invalidOp VarAlreadySetOrLinked

        member t.TrySet(x) =
          match t.state with
          | Pending -> t.state <- Value x; true
          | PendingOne (supervisor, f) -> t.state <- Value x; enqueue supervisor f x; true
          | PendingMany callbacks ->
            t.state <- Value x
            for (supervisor, f) in RegistrationList.toList callbacks do enqueue supervisor f x
            true
          | _ -> false

        member t.Link(parent) =
          if not ((t :> _ IVar).TryLink(parent)) then
            invalidOp VarAlreadySetOrLinked

        member t.TryLink(parent) =
          let inline findRootAndLink action =
            let root = T<'a>.FindRoot(parent)
            if obj.ReferenceEquals(root, t) then
              // This is a cycle.
              t.state <- Linked (Never.never ())
            else
              t.state <- Linked root
              action root
          match t.state with
          | Pending -> findRootAndLink ignore; true
          | PendingOne callback -> findRootAndLink (fun root -> upon' root callback); true
          | PendingMany callbacks -> findRootAndLink (fun root -> moveFrom root callbacks); true
          | _ -> false

    let inline createPending () = {state = Pending}

    let inline createLinked d = {state = Linked d}

    let inline createValue x = {state = Value x}

  let value x = Var.createValue x :> _ IDeferred

  let createVar () = Var.createPending () :> _ IVar

  let inline create f =
    let v = createVar ()
    f v
    v :> _ IDeferred

  let unit = value ()

  let never () = Never.never () :> _ IDeferred

  let inline createThreadStart v f =
    let supervisor = ThreadShared.currentSupervisor ()
    (fun _ ->
      try
        let x = f ()
        supervisor.Dispatcher.Enqueue(supervisor, fun () -> set v x)
      with ex ->
        supervisor.Dispatcher.Enqueue(supervisor, fun () -> supervisor.SendException(ex))
    )

  let start f =
    create (fun v ->
      ThreadPool.QueueUserWorkItem(new WaitCallback(createThreadStart v f)) |> ignore
    )

  let startThread threadType f =
    create (fun v ->
      let thread = new Thread(new ParameterizedThreadStart(createThreadStart v f))
      thread.IsBackground <-
        match threadType with
        | ThreadType.Foreground -> false
        | ThreadType.Background -> true
      thread.Start()
    )

  let ``return`` x = value x

  let bind t (f : _ -> _ IDeferred) = create (fun v -> upon t (fun x -> link v (f x)))

  let map t f = create (fun v -> upon t (fun x -> set v (f x)))

  let join t = bind t id

  let forget t = map t ignore

  let inline foldiList (f : _ -> _ -> _ -> _ IDeferred) state xs =
    match xs with
    | [] -> value state
    | x :: xs ->
      create (fun v ->
        let rec loop i state x xs =
          match xs with
          | [] -> link v (f i state x)
          | x' :: xs' -> upon (f i state x) (fun state -> loop (i + 1) state x' xs')
        loop 0 state x xs
      )

  let inline mapiListSequential (f : _ -> _ -> _ IDeferred) xs =
    map (foldiList (fun i acc x -> map (f i x) (fun y -> y :: acc)) [] xs) List.rev

  let all ts = mapiListSequential (fun i t -> t) ts

  let allForget ts = foldiList (fun i () t -> t) () ts

  module Infix =
    let inline (>>=) t f = bind t f
    let inline (>>|) t f = map t f
    let inline (>>>) t f = upon t f
    let inline (<--) var value = set var value
    let inline (--<) var d = link var d

  let allUnit ts = allForget ts

  let both (t1 : 'a IDeferred) (t2 : 'b IDeferred) =
    create (fun v -> upon t1 (fun x1 -> upon t2 (fun x2 -> set v (x1, x2))))

  let inline mapAnyi (ts : _ IDeferred list) f =
    create (fun v ->
      let dispatcher = ThreadShared.currentDispatcher ()
      let mutable registrations = []
      registrations <- ts |> List.mapi (fun i t ->
        register' t (dispatcher.RootSupervisor, fun x ->
          if not v.IsDetermined then
            for registration in registrations do Registration.remove registration
            set v (f i x)
        )
      )
    )

  let anyi (ts : _ IDeferred list) = mapAnyi ts (fun i x -> (i, x))

  let any (ts : _ IDeferred list) = mapAnyi ts (fun i x -> x)

  let anyUnit (ts : _ IDeferred list) = mapAnyi ts (fun i x -> ())

  let inline dontWaitFor (t : unit IDeferred) = ()

  // We define tryFinally in this module because we need it for sequence processing later on.
  let tryFinally (f : unit -> _ IDeferred) (finalizer : unit -> _ IDeferred) =
    let supervisor = ThreadShared.currentSupervisor ()
    let childSupervisor = ChildSupervisor.create "tryFinally"
    childSupervisor.Detach()
    let raiseAfterDetermined ex = raise (AfterDeterminedException (childSupervisor.Name, ex))
    let mutable ex = null
    let d =
      try
        childSupervisor.Run(f)
      with ex' ->
        ex <- ex'
        Unchecked.defaultof<_ IDeferred>
    let ex = ex // Immutable from here
    match ex with
    | null ->
      if isDetermined d then
        childSupervisor.UponException(raiseAfterDetermined)
        map (finalizer ()) (fun () -> get d)
      else
        let reader = createVar ()
        let mutable writer = Some reader
        childSupervisor.UponException(fun ex ->
          match writer with
          | Some _ -> writer <- None; upon (finalizer ()) (fun () -> supervisor.SendException(ex))
          | None -> raiseAfterDetermined ex
        )
        upon d (fun x ->
          match writer with
          | Some v -> writer <- None; upon (finalizer ()) (fun () -> set v x)
          | None -> ()
        )
        reader :> _ IDeferred
    | _ ->
      childSupervisor.UponException(raiseAfterDetermined)
      upon (finalizer ()) (fun () -> supervisor.SendException(ex))
      never ()

  let ofAsyncStart a =
    let supervisor = ThreadShared.currentSupervisor ()
    create (fun v ->
      Async.Start(async {
        try
          let! x = a
          supervisor.Dispatcher.Enqueue(supervisor, fun () -> set v x)
        with ex ->
          supervisor.Dispatcher.Enqueue(supervisor, fun () -> supervisor.SendException(ex))
      })
    )

  let inline internal ofBeginEnd (``begin`` : AsyncCallback -> _) ``end`` =
    let supervisor = ThreadShared.currentSupervisor ()
    create (fun v ->
      ``begin`` (new AsyncCallback(fun result ->
        supervisor.Dispatcher.Enqueue(supervisor, fun () -> set v (``end`` result))
      )) |> ignore
    )

  let ofTask (task : 'a Task) =
    create (fun v ->
      let supervisor = ThreadShared.currentSupervisor ()
      task.ContinueWith(Action<Task<'a>>(fun _ ->
        supervisor.Dispatcher.Enqueue(supervisor, fun () ->
          match task.Exception with
          | null -> v.Set(task.Result)
          | _ ->
            match task.Exception.InnerException with
            | null -> supervisor.SendException(task.Exception)
            | _ -> supervisor.SendException(task.Exception.InnerException)
        )
      )) |> ignore
    )

  let ofTaskUnit (task : Task) =
    create (fun v ->
      let supervisor = ThreadShared.currentSupervisor ()
      task.ContinueWith(Action<Task>(fun _ ->
        supervisor.Dispatcher.Enqueue(supervisor, fun () ->
          match task.Exception with
          | null ->
            // We need special handling for cancellation because Task does not raise for
            // OperationCanceledException (even though Task<'a> does).
            if task.IsCanceled then
              raise (new OperationCanceledException())
            else
              v.Set(())
          | _ ->
            match task.Exception.InnerException with
            | null -> supervisor.SendException(task.Exception)
            | _ -> supervisor.SendException(task.Exception.InnerException)
        )
      )) |> ignore
    )

  open Infix

  [<Interface>]
  type 'b IChoice =
    abstract member Register : supervisedCallback : unit SupervisedCallback -> IRegistration
    abstract member TryGetApply : unit -> 'b option

  module Choice =
    [<ReferenceEquality>]
    type T<'a, 'b> =
      | Choice of deferred : 'a IDeferred * mapping : ('a -> 'b)

      interface 'b IChoice with
        member t.Register((supervisor, callback)) =
          match t with Choice (d, _) -> d.Register((supervisor, ignore >> callback))

        member t.TryGetApply() = match t with Choice (d, f) -> tryGet d |> Option.map f

    let create d f = Choice (d, f)

  let choice d f = Choice.create d f :> _ IChoice

  let choose (choices : _ IChoice list) =
    create (fun v ->
      let rec setResult (cs : _ IChoice list) =
        match cs with
        | [] -> assert false
        | c :: cs ->
          match c.TryGetApply() with
          | Some y -> v <-- y
          | None -> setResult cs
      let mutable registrations = []
      registrations <- choices |> List.map (fun c ->
        c.Register(ThreadShared.currentSupervisor (), fun () ->
          if not v.IsDetermined then
            for registration in registrations do Registration.remove registration
            setResult choices
        )
      )
    )

  module Repeat =
    [<ReferenceEquality>]
    type T<'state, 'a> = Repeat of 'state | Done of 'a

  let repeat (f : _ -> Repeat.T<_, _> IDeferred) state =
    create (fun v ->
      let rec loop state =
        f state >>> (function
          | Repeat.Repeat state -> loop state
          | Repeat.Done x -> v <-- x
        )
      loop state
    )

  let repeatForever (f : _ -> _ IDeferred) state =
    let rec loop state = f state >>> (fun state -> loop state)
    loop state

  type ConcurrentConcurrency =
    | Unique

    interface IConcurrency with
      member this.Enqueue(f) = f ()

  type SequentialConcurrency() =
    let tasks = Queue.create ()
    let mutable busy = false

    member inline this.StartTask(f) =
      busy <- true
      f ()

    member inline this.CompletedTask() =
      busy <- false
      match Queue.tryDequeue tasks with
      | Some f -> this.StartTask(f)
      | None -> ()

    interface IConcurrency with
      member this.Enqueue(f) =
        if Queue.isEmpty tasks && not busy then
          this.StartTask(fun () ->
            let d = f ()
            d >>> (fun _ -> this.CompletedTask())
            d
          )
        else
          create (fun v ->
            Queue.enqueue tasks (fun () ->
              f () >>> (fun x -> v <-- x; this.CompletedTask())
            )
          )

  type LocallySequentialConcurrency private() =
    inherit SequentialConcurrency()

    static member val Unique = new LocallySequentialConcurrency()

  let concurrently (c : IConcurrency) f =
    match c with
    | :? ConcurrentConcurrency -> f
    | _ -> (fun x -> c.Enqueue(fun () -> f x))

  let concurrently2 (c : IConcurrency) f =
    match c with
    | :? ConcurrentConcurrency -> f
    | _ -> (fun x y -> c.Enqueue(fun () -> f x y))

  module Array =
    let inline foldiInline (f : _ -> _ -> _ -> _ IDeferred) state xs =
      let n = Array.length xs
      if n = 0 then
        value state
      else
        create (fun v ->
          let rec loop i state =
            if i = n - 1 then
              v --< f i state xs.[i]
            else
              f i state xs.[i] >>> (fun state -> loop (i + 1) state)
          loop 0 state
        )

    let foldi f state xs = foldiInline f state xs

    let fold f state xs = foldiInline (fun i args -> f args) state xs

    let inline mapiSequential (f : _ -> _ -> _ IDeferred) xs =
      let length = Array.length xs
      if length = 0 then
        value Array.empty
      else
        let ys = Array.zeroCreate length
        foldiInline (fun i ys x -> f i x >>| (fun y -> ys.[i] <- y; ys)) ys xs

    let all ts = mapiSequential (fun i t -> t) ts

    let allUnit ts = foldiInline (fun i () t -> t) () ts

    let iteri (c : IConcurrency) f xs =
      match c with
      | :? LocallySequentialConcurrency -> xs |> foldiInline (fun i () x -> f i x) ()
      | _ -> xs |> Array.mapi (concurrently2 c f) |> allUnit

    let iter c f xs = iteri c (fun i args -> f args) xs

    let mapi (c : IConcurrency) f xs =
      match c with
      | :? LocallySequentialConcurrency -> mapiSequential f xs
      | _ -> xs |> Array.mapi (concurrently2 c f) |> all

    let map c f xs = mapi c (fun i args -> f args) xs

    let init c length f = Array.init length id |> map c f

    let collect c f xs = map c f xs >>| Array.concat

    let choose c f xs = map c f xs >>| Array.choose id

    let filter2 xs bs =
      let list = new System.Collections.Generic.List<_>()
      Array.iter2 (fun x b -> if b then list.Add(x)) xs bs
      list.ToArray()

    let filter c f xs = map c f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) xs =
      let n = Array.length xs
      if n = 0 then
        value None
      else
        create (fun v ->
          let rec loop i =
            if i = n - 1 then
              v --< f xs.[i]
            else
              f xs.[i] >>> (fun y -> match y with Some _ -> v <-- y | None -> loop (i + 1))
          loop 0
        )

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  module List =
    let foldi f state xs = foldiList f state xs

    let fold f state xs = foldiList (fun i args -> f args) state xs

    let all ts = all ts

    let allUnit ts = allUnit ts

    let iteri (c : IConcurrency) f xs =
      match c with
      | :? LocallySequentialConcurrency -> xs |> foldiList (fun i () x -> f i x) ()
      | _ -> xs |> List.mapi (concurrently2 c f) |> allUnit

    let iter c f xs = iteri c (fun i args -> f args) xs

    let mapi (c : IConcurrency) f xs =
      match c with
      | :? LocallySequentialConcurrency -> mapiListSequential f xs
      | _ -> xs |> List.mapi (concurrently2 c f) |> all

    let map c f xs = mapi c (fun i args -> f args) xs

    let init c length f = List.init length id |> map c f

    let collect c f xs = map c f xs >>| List.concat

    let choose c f xs = map c f xs >>| List.choose id

    let filter2 xs bs = List.foldBack2 (fun x b acc -> if b then x :: acc else acc) xs bs []

    let filter c f xs = map c f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) xs =
      match xs with
      | [] -> value None
      | x :: xs ->
        create (fun v ->
          let rec loop x xs =
            match xs with
            | [] -> v --< f x
            | x' :: xs'' -> f x >>> (fun y -> match y with Some _ -> v <-- y | None -> loop x' xs'')
          loop x xs
        )

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  module Seq =
    let foldi (f : _ -> _ -> _ -> _ IDeferred) state (xs : _ seq) =
      match xs with
      | :? (_ list) as xs -> List.foldi f state xs
      | :? (_ array) as xs -> Array.foldi f state xs
      | _ ->
        let e = xs.GetEnumerator()
        tryFinally (fun () ->
          create (fun v ->
            let rec loop i state =
              if e.MoveNext() then
                f i state e.Current >>> (fun state -> loop (i + 1) state)
              else
                v <-- state
            loop 0 state
          )
        ) (fun () -> e.Dispose(); unit)

    let fold f state xs = foldi (fun i args -> f args) state xs

    let inline mapiSequential (f : _ -> _ -> _ IDeferred) xs =
      let list = new System.Collections.Generic.List<_>()
      let ys = list :> _ seq
      foldi (fun i ys x -> f i x >>| (fun y -> list.Add(y); ys)) ys xs

    let all ts = mapiSequential (fun i t -> t) ts

    let allUnit ts = foldi (fun i () t -> t) () ts

    let iteri (c : IConcurrency) f (xs : _ seq) =
      match xs with
      | :? (_ list) as xs -> List.iteri c f xs
      | :? (_ array) as xs -> Array.iteri c f xs
      | _ ->
        match c with
        | :? LocallySequentialConcurrency -> xs |> foldi (fun i () x -> f i x) ()
        | _ -> xs |> Seq.mapi (concurrently2 c f) |> allUnit

    let iter c f xs = iteri c (fun i args -> f args) xs

    let mapi (c : IConcurrency) f (xs : _ seq) =
      // TODO: Remove upcasts when xs is a list or array, after F# gets support for covariance in
      // generics.
      match xs with
      | :? (_ list) as xs -> List.mapi c f xs >>| (fun ys -> ys :> _ seq)
      | :? (_ array) as xs -> Array.mapi c f xs >>| (fun ys -> ys :> _ seq)
      | _ ->
        match c with
        | :? LocallySequentialConcurrency -> mapiSequential f xs
        | _ -> xs |> Seq.mapi (concurrently2 c f) |> all

    let map c f xs = mapi c (fun i args -> f args) xs

    let init c length f = Seq.init length id |> map c f

    let collect c f xs = map c f xs >>| Seq.concat

    let choose c f xs = map c f xs >>| Seq.choose id

    let filter2 xs bs =
      let list = new System.Collections.Generic.List<_>()
      Seq.iter2 (fun x b -> if b then list.Add(x)) xs bs
      list :> _ seq

    let filter c f xs = map c f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) (xs : _ seq) =
      match xs with
      | :? (_ list) as xs -> List.tryPick f xs
      | :? (_ array) as xs -> Array.tryPick f xs
      | _ ->
        let e = xs.GetEnumerator()
        tryFinally (fun () ->
          create (fun v ->
            let rec loop () =
              if e.MoveNext() then
                f e.Current >>> (fun y -> match y with Some _ -> v <-- y | None -> loop ())
              else
                v <-- None
            loop ()
          )
        ) (fun () -> e.Dispose(); unit)

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))
