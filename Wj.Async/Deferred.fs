namespace Wj.Async

open System
open System.Threading.Tasks

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

  let enqueue (supervisor : ISupervisor) f x =
    supervisor.Dispatcher.Enqueue((supervisor, fun () -> f x))

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
      /// The Var is not linked and does not have a value.
      | Pending of callbacks : 'a SupervisedCallback RegistrationList.T
      /// The Var is set to a value.
      | Value of value : 'a
      /// The Var is linked to a parent IDeferred.
      /// Due to FindRoot, the parent will always be a Pending, Value or not a Var.T at all (but
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
          lock t (fun () ->
            match t.state with
            | Pending callbacks -> RegistrationList.add callbacks supervisedCallback
            | Value x -> enqueue supervisor f x
            | Linked parent -> upon' (T<'a>.FindRoot(parent)) supervisedCallback
          )

        member t.Register(f) = (t :> _ IDeferred).Register((ThreadShared.currentSupervisor (), f))

        member t.Register((supervisor, f) as supervisedCallback) =
          lock t (fun () ->
            match t.state with
            | Pending callbacks -> RegistrationList.register callbacks supervisedCallback
            | Value x -> enqueue supervisor f x; Registration.empty
            | Linked parent -> register' (T<'a>.FindRoot(parent)) supervisedCallback
          )

        member t.MoveFrom(from) =
          match t.state with
          | Pending callbacks -> RegistrationList.moveFrom callbacks from
          | Value x ->
            RegistrationList.toList from
            |> List.iter (fun (supervisor, f) -> enqueue supervisor f x)
          | Linked parent -> moveFrom (T<'a>.FindRoot(parent)) from

        member t.IsDetermined =
          match t.state with
          | Pending _ -> false
          | Value _ -> true
          | Linked parent -> isDetermined (T<'a>.FindRoot(parent))

        member t.Get() =
          match t.state with
          | Pending _ -> invalidOp DeferredNotDetermined
          | Value x -> x
          | Linked parent -> get (T<'a>.FindRoot(parent))

        member t.TryGet() =
          match t.state with
          | Pending _ -> None
          | Value x -> Some x
          | Linked parent -> tryGet (T<'a>.FindRoot(parent))

      interface 'a IVar with
        member t.Set(x) =
          if not ((t :> 'a IVar).TrySet(x)) then
            invalidOp VarAlreadySetOrLinked

        member t.TrySet(x) =
          lock t (fun () ->
            match t.state with
            | Pending callbacks ->
              t.state <- Value x
              RegistrationList.toList callbacks
              |> List.iter (fun (supervisor, f) -> enqueue supervisor f x)
              true
            | _ -> false
          )

        member t.Link(parent) =
          if not ((t :> _ IVar).TryLink(parent)) then
            invalidOp VarAlreadySetOrLinked

        member t.TryLink(parent) =
          match t.state with
          | Pending callbacks ->
            let root = T<'a>.FindRoot(parent)
            if obj.ReferenceEquals(root, t) then
              // This is a cycle.
              t.state <- Linked (Never.never ())
            else
              t.state <- Linked root
              moveFrom root callbacks
            true
          | _ -> false

    let inline createPending () = {state = Pending (RegistrationList.create ())}

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
    let inline (-->) value var = set var value
    let inline (>--) d var = link var d

  let allUnit ts = allForget ts

  let both (t1 : 'a IDeferred) (t2 : 'b IDeferred) =
    create (fun v -> upon t1 (fun x1 -> upon t2 (fun x2 -> set v (x1, x2))))

  let mapAnyi (ts : _ IDeferred list) f =
    create (fun v ->
      let dispatcher = ThreadShared.currentDispatcher ()
      let mutable registrations = []
      registrations <- ts |> List.mapi (fun i t ->
        register' t (dispatcher.RootSupervisor, (fun x ->
          if not v.IsDetermined then
            registrations |> List.iter Registration.remove
            set v (f i x)
        ))
      )
    )

  let anyi (ts : _ IDeferred list) = mapAnyi ts (fun i x -> (i, x))

  let any (ts : _ IDeferred list) = mapAnyi ts (fun i x -> x)

  let anyUnit (ts : _ IDeferred list) = mapAnyi ts (fun i x -> ())

  let dontWaitFor (t : unit IDeferred) = ()

  // We define tryFinally in this module because we need it for sequence processing later on.
  let tryFinally (f : unit -> _ IDeferred) (finalizer : unit -> _ IDeferred) =
    let t = ChildSupervisor.create "tryFinally"
    t.Detach()
    let result = t.TryRun(f)
    match result with
    | Result.Success d ->
      if isDetermined d then
        t.UponException(raise)
        map (finalizer ()) (fun () -> get d)
      else
        let reader = createVar ()
        let mutable writer = Some reader
        t.UponException(fun ex ->
          match writer with
          | Some _ -> writer <- None; upon (finalizer ()) (fun () -> raise ex)
          | None -> raise ex
        )
        upon d (fun x ->
          match writer with
          | Some v -> writer <- None; upon (finalizer ()) (fun () -> set v x)
          | None -> ()
        )
        reader :> _ IDeferred
    | Result.Failure ex ->
      t.UponException(raise)
      upon (finalizer ()) (fun () -> raise ex)
      never ()

  let ofAsync a =
    let v = createVar ()
    let a = async {
      let! x = a
      set v x
    }
    (a, v :> _ IDeferred)

  let ofTask (task : 'a Task) =
    create (fun v -> task.ContinueWith(Action<Task<'a>>(fun _ -> v.Set(task.Result))) |> ignore)

  let ofTaskUnit (task : Task) =
    create (fun v -> task.ContinueWith(Action<Task>(fun _ -> v.Set(()))) |> ignore)

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
          | Some y -> y --> v
          | None -> setResult cs
      let mutable registrations = []
      registrations <- choices |> List.map (fun c ->
        c.Register(ThreadShared.currentSupervisor (), (fun () ->
          if not v.IsDetermined then
            registrations |> List.iter Registration.remove
            setResult choices
        ))
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
          | Repeat.Done x -> x --> v
        )
      loop state
    )

  let repeatForever (f : _ -> _ IDeferred) state =
    let rec loop state = f state >>> (fun state -> loop state)
    loop state

  module Array =
    module P = Parallelism

    let inline foldiInline (f : _ -> _ -> _ -> _ IDeferred) state xs =
      let n = Array.length xs
      if n = 0 then
        value state
      else
        create (fun v ->
          let rec loop i state =
            if i = n - 1 then
              f i state xs.[i] >-- v
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

    let iteri p f xs =
      match p with
      | P.Sequential -> xs |> foldiInline (fun i () x -> f i x) ()
      | P.Parallel -> xs |> Array.mapi (fun i x -> f i x) |> allUnit

    let iter p f xs = iteri p (fun i args -> f args) xs

    let mapi p f xs =
      match p with
      | P.Sequential -> mapiSequential f xs
      | P.Parallel -> xs |> Array.mapi (fun i x -> f i x) |> all

    let map p f xs = mapi p (fun i args -> f args) xs

    let init p length f = Array.init length id |> map p f

    let collect p f xs = map p f xs >>| Array.concat

    let choose p f xs = map p f xs >>| Array.choose id

    let filter2 xs bs =
      let list = new System.Collections.Generic.List<_>()
      Array.iter2 (fun x b -> if b then list.Add(x)) xs bs
      list.ToArray()

    let filter p f xs = map p f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) xs =
      let n = Array.length xs
      if n = 0 then
        value None
      else
        create (fun v ->
          let rec loop i =
            if i = n - 1 then
              f xs.[i] >-- v
            else
              f xs.[i] >>> (fun y -> match y with Some _ -> y --> v | None -> loop (i + 1))
          loop 0
        )

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  module List =
    module P = Parallelism

    let foldi f state xs = foldiList f state xs

    let fold f state xs = foldiList (fun i args -> f args) state xs

    let all ts = all ts

    let allUnit ts = allUnit ts

    let iteri p f xs =
      match p with
      | P.Sequential -> xs |> foldiList (fun i () x -> f i x) ()
      | P.Parallel -> xs |> List.mapi (fun i x -> f i x) |> allUnit

    let iter p f xs = iteri p (fun i args -> f args) xs

    let mapi p f xs =
      match p with
      | P.Sequential -> mapiListSequential f xs
      | P.Parallel -> xs |> List.mapi (fun i x -> f i x) |> all

    let map p f xs = mapi p (fun i args -> f args) xs

    let init p length f = List.init length id |> map p f

    let collect p f xs = map p f xs >>| List.concat

    let choose p f xs = map p f xs >>| List.choose id

    let filter2 xs bs = List.foldBack2 (fun x b acc -> if b then x :: acc else acc) xs bs []

    let filter p f xs = map p f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) xs =
      match xs with
      | [] -> value None
      | x :: xs ->
        create (fun v ->
          let rec loop x xs =
            match xs with
            | [] -> f x >-- v
            | x' :: xs'' -> f x >>> (fun y -> match y with Some _ -> y --> v | None -> loop x' xs'')
          loop x xs
        )

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  module Seq =
    module P = Parallelism

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
                state --> v
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

    let iteri p f (xs : _ seq) =
      match xs with
      | :? (_ list) as xs -> List.iteri p f xs
      | :? (_ array) as xs -> Array.iteri p f xs
      | _ ->
        match p with
        | P.Sequential -> xs |> foldi (fun i () x -> f i x) ()
        | P.Parallel -> xs |> Seq.mapi (fun i x -> f i x) |> allUnit

    let iter p f xs = iteri p (fun i args -> f args) xs

    let mapi p f (xs : _ seq) =
      // TODO: Remove upcasts when xs is a list or array, after F# gets support for covariance in
      // generics.
      match xs with
      | :? (_ list) as xs -> List.mapi p f xs >>| (fun ys -> ys :> _ seq)
      | :? (_ array) as xs -> Array.mapi p f xs >>| (fun ys -> ys :> _ seq)
      | _ ->
        match p with
        | P.Sequential -> mapiSequential f xs
        | P.Parallel -> xs |> Seq.mapi (fun i x -> f i x) |> all

    let map p f xs = mapi p (fun i args -> f args) xs

    let init p length f = Seq.init length id |> map p f

    let collect p f xs = map p f xs >>| Seq.concat

    let choose p f xs = map p f xs >>| Seq.choose id

    let filter2 xs bs =
      let list = new System.Collections.Generic.List<_>()
      Seq.iter2 (fun x b -> if b then list.Add(x)) xs bs
      list :> _ seq

    let filter p f xs = map p f xs >>| filter2 xs

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
                f e.Current >>> (fun y -> match y with Some _ -> y --> v | None -> loop ())
              else
                None --> v
            loop ()
          )
        ) (fun () -> e.Dispose(); unit)

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))
