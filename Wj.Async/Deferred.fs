namespace Wj.Async

open System
open System.Threading.Tasks

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
  let isDetermined (t : _ IDeferred) = t.IsDetermined
  let get (t : _ IDeferred) = t.Get()
  let tryGet (t : _ IDeferred) = t.TryGet()
  // IVar functions
  let set (t : _ IVar) x = t.Set(x)
  let trySet (t : _ IVar) x = t.TrySet(x)
  // INode functions
  let isLinked (t : _ INode) = t.IsLinked
  let link (t : _ INode) d = t.Link(d)
  let tryLink (t : _ INode) d = t.TryLink(d)

  let enqueue (supervisor : ISupervisor) f x =
    supervisor.Dispatcher.Enqueue((supervisor, fun () -> f x))

  module private Var =
    [<ReferenceEquality>]
    type 'a State =
      | Pending of callbacks : 'a SupervisedCallback RegistrationList.T
      | Done of value : 'a

    [<ReferenceEquality>]
    type 'a T =
      {mutable state : 'a State}

      interface 'a IDeferred with
        member t.Upon(f) = (t :> _ IDeferred).Upon((ThreadShared.currentSupervisor (), f))

        member t.Upon((supervisor, f) as supervisedCallback) =
          match t.state with
          | Pending callbacks -> RegistrationList.add callbacks supervisedCallback
          | Done x -> enqueue supervisor f x

        member t.Register(f) = (t :> _ IDeferred).Register((ThreadShared.currentSupervisor (), f))

        member t.Register((supervisor, f) as supervisedCallback) =
          match t.state with
          | Pending callbacks -> RegistrationList.register callbacks supervisedCallback
          | Done x -> enqueue supervisor f x; Registration.empty

        member t.MoveFrom(from) =
          match t.state with
          | Pending callbacks -> RegistrationList.moveFrom callbacks from
          | Done x ->
            RegistrationList.toList from
            |> List.iter (fun (supervisor, f) -> enqueue supervisor f x)

        member t.IsDetermined =
          match t.state with
          | Pending _ -> false
          | Done _ -> true

        member t.Get() =
          match t.state with
          | Pending _ -> invalidOp DeferredNotDetermined
          | Done x -> x

        member t.TryGet() =
          match t.state with
          | Pending _ -> None
          | Done x -> Some x

      interface 'a IVar with
        member t.Set(x) =
          if not ((t :> 'a IVar).TrySet(x)) then
            invalidOp VarAlreadySet

        member t.TrySet(x) =
          match t.state with
          | Pending callbacks ->
            t.state <- Done x
            RegistrationList.toList callbacks
            |> List.iter (fun (supervisor, f) -> enqueue supervisor f x)
            true
          | Done _ -> false

    let createPending () = {state = Pending (RegistrationList.create ())}

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

        member t.IsDetermined = false

        member t.Get() = invalidOp DeferredNotDetermined

        member t.TryGet() = None

    let create () = Never

  module private Node =
    [<ReferenceEquality>]
    type 'a State =
      /// The node has no parent.
      | Unlinked of callbacks : 'a SupervisedCallback RegistrationList.T
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

        member t.IsDetermined =
          match t.state with
          | Unlinked _ -> false
          | Linked parent -> isDetermined (T<'a>.FindRoot(parent))

        member t.Get() =
          match t.state with
          | Unlinked _ -> invalidOp DeferredNotDetermined
          | Linked parent -> get (T<'a>.FindRoot(parent))

        member t.TryGet() =
          match t.state with
          | Unlinked _ -> None
          | Linked parent -> tryGet (T<'a>.FindRoot(parent))

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

  let value x = Var.createDone x :> _ IDeferred

  let createVar () = Var.createPending () :> _ IVar

  let createNode () = Node.create () :> _ INode

  let unit = value ()

  let never () = Never.create () :> _ IDeferred

  let ``return`` x = value x

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

  let inline foldiList (f : _ -> _ -> _ -> _ IDeferred) state xs =
    match xs with
    | [] -> value state
    | x :: xs ->
      let v = createNode ()
      let rec loop i state x xs =
        match xs with
        | [] -> link v (f i state x)
        | x' :: xs' -> upon (f i state x) (fun state -> loop (i + 1) state x' xs')
      loop 0 state x xs
      v :> _ IDeferred

  let inline mapiListSequential (f : _ -> _ -> _ IDeferred) xs =
    map (foldiList (fun i acc x -> map (f i x) (fun y -> y :: acc)) [] xs) List.rev

  let all ts = mapiListSequential (fun i t -> t) ts

  let allForget ts = foldiList (fun i () t -> t) () ts

  module Infix =
    let (>>=) t f = bind t f
    let (>>|) t f = map t f
    let (>>>) t f = upon t f
    let (>--) t node = link node t

  let allUnit ts = allForget ts

  let both (t1 : 'a IDeferred) (t2 : 'b IDeferred) =
    let v = createVar ()
    upon t1 (fun x1 -> upon t2 (fun x2 -> set v (x1, x2)))
    v :> _ IDeferred

  let mapAnyi (ts : _ IDeferred list) f =
    let dispatcher = ThreadShared.currentDispatcher ()
    let v = createVar ()
    let mutable registrations = []
    registrations <- ts |> List.mapi (fun i t ->
      register' t (dispatcher.RootSupervisor, (fun x ->
        if not v.IsDetermined then
          registrations |> List.iter Registration.remove
          set v (f i x)
      ))
    )
    v :> _ IDeferred

  let anyi (ts : _ IDeferred list) = mapAnyi ts (fun i x -> (i, x))

  let any (ts : _ IDeferred list) = mapAnyi ts (fun i x -> x)

  let anyUnit (ts : _ IDeferred list) = mapAnyi ts (fun i x -> ())

  let dontWaitFor (t : unit IDeferred) = ()

  // We define tryFinally in this module because we need it for sequence processing later on.
  let tryFinally (f : unit -> _ IDeferred) (finalizer : unit -> _ IDeferred) =
    let t = ChildSupervisor.create "tryFinally"
    t.Detach()
    let result = t.Run(f)
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
          | Some _ ->
            writer <- None
            upon (finalizer ()) (fun () -> raise ex)
          | None -> raise ex
        )
        upon d (fun x ->
          match writer with
          | Some v ->
            writer <- None
            upon (finalizer ()) (fun () -> set v x)
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
    let v = createVar ()
    task.ContinueWith(Action<Task<'a>>(fun _ -> v.Set(task.Result))) |> ignore
    v :> _ IDeferred

  let ofTaskUnit (task : Task) =
    let v = createVar ()
    task.ContinueWith(Action<Task>(fun _ -> v.Set(()))) |> ignore
    v :> _ IDeferred

  [<Interface>]
  type 'b IChoice =
    abstract member Register : supervisedCallback : unit SupervisedCallback -> IRegistration
    abstract member TryGetApply : unit -> 'b option

  module Choice =
    type T<'a, 'b> =
      | Choice of deferred : 'a IDeferred * mapping : ('a -> 'b)

      interface 'b IChoice with
        member t.Register((supervisor, callback)) =
          match t with Choice (d, _) -> d.Register((supervisor, ignore >> callback))

        member t.TryGetApply() = match t with Choice (d, f) -> tryGet d |> Option.map f

    let create d f = Choice (d, f)

  let choice d f = Choice.create d f :> _ IChoice

  let choose (choices : _ IChoice list) =
    let v = createVar ()
    let rec setResult (cs : _ IChoice list) =
      match cs with
      | [] -> assert false
      | c :: cs ->
        match c.TryGetApply() with
        | Some y -> set v y
        | None -> setResult cs
    let mutable registrations = []
    registrations <- choices |> List.map (fun c ->
      c.Register(ThreadShared.currentSupervisor (), (fun () ->
        if not v.IsDetermined then
          registrations |> List.iter Registration.remove
          setResult choices
      ))
    )
    v :> _ IDeferred

  module Repeat =
    type T<'state, 'a> = Repeat of 'state | Done of 'a

  let repeat (f : _ -> Repeat.T<_, _> IDeferred) state =
    let v = createVar ()
    let rec loop state =
      upon (f state) (fun r ->
        match r with
        | Repeat.Repeat state -> loop state
        | Repeat.Done x -> set v x
      )
    loop state
    v :> _ IDeferred

  let repeatForever (f : _ -> _ IDeferred) state =
    let rec loop state = upon (f state) (fun state -> loop state)
    loop state

  open Infix

  module Array =
    module P = Parallelism

    let foldi (f : _ -> _ -> _ -> _ IDeferred) state xs =
      let n = Array.length xs
      if n = 0 then
        value state
      else
        let v = createNode ()
        let rec loop i state =
          if i = n - 1 then
            f i state xs.[i] >-- v
          else
            f i state xs.[i] >>> (fun state -> loop (i + 1) state)
        loop 0 state
        v :> _ IDeferred

    let fold f state xs = foldi (fun i args -> f args) state xs

    let inline mapiSequential (f : _ -> _ -> _ IDeferred) xs =
      let length = Array.length xs
      if length = 0 then
        value Array.empty
      else
        let ys = Array.zeroCreate length
        foldi (fun i ys x -> f i x >>| (fun y -> ys.[i] <- y; ys)) ys xs

    let all ts = mapiSequential (fun i t -> t) ts

    let allUnit ts = foldi (fun i () t -> t) () ts

    let iteri p f xs =
      match p with
      | P.Sequential -> xs |> foldi (fun i () x -> f i x) ()
      | P.Parallel -> xs |> Array.mapi (fun i x -> f i x) |> allUnit

    let iter p f xs = iteri p (fun i args -> f args) xs

    let mapi p f xs =
      match p with
      | P.Sequential -> mapiSequential f xs
      | P.Parallel -> xs |> Array.mapi (fun i x -> f i x) |> all

    let map p f xs = mapi p (fun i args -> f args) xs

    let init p length f = Array.init length id |> map p f

    let concatMap p f xs = map p f xs >>| Array.concat

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
        let v = createNode ()
        let rec loop i =
          if i = n - 1 then
            f xs.[i] >-- v
          else
            let d = f xs.[i]
            d >>> (fun y -> match y with Some _ -> d >-- v | None -> loop (i + 1))
        loop 0
        v :> _ IDeferred

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  module List =
    module P = Parallelism

    let foldi f state xs = foldiList f state xs

    let fold f state xs = foldi (fun i args -> f args) state xs

    let all ts = all ts

    let allUnit ts = allUnit ts

    let iteri p f xs =
      match p with
      | P.Sequential -> xs |> foldi (fun i () x -> f i x) ()
      | P.Parallel -> xs |> List.mapi (fun i x -> f i x) |> allUnit

    let iter p f xs = iteri p (fun i args -> f args) xs

    let mapi p f xs =
      match p with
      | P.Sequential -> mapiListSequential f xs
      | P.Parallel -> xs |> List.mapi (fun i x -> f i x) |> all

    let map p f xs = mapi p (fun i args -> f args) xs

    let init p length f = List.init length id |> map p f

    let concatMap p f xs = map p f xs >>| List.concat

    let choose p f xs = map p f xs >>| List.choose id

    let filter2 xs bs = List.fold2 (fun acc x b -> if b then x :: acc else acc) [] xs bs |> List.rev

    let filter p f xs = map p f xs >>| filter2 xs

    let tryPick (f : _ -> _ option IDeferred) xs =
      match xs with
      | [] -> value None
      | x :: xs ->
        let v = createNode ()
        let rec loop x xs =
          match xs with
          | [] -> f x >-- v
          | x' :: xs'' ->
            let d = f x
            d >>> (fun y -> match y with Some _ -> d >-- v | None -> loop x' xs'')
        loop x xs
        v :> _ IDeferred

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
          if e.MoveNext() then
            let v = createNode ()
            let rec loop i state x =
              if e.MoveNext() then
                f i state x >>> (fun state -> loop (i + 1) state e.Current)
              else
                f i state x >-- v
            loop 0 state e.Current
            v :> _ IDeferred
          else
            value state
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

    let concatMap p (f : _ -> _ seq IDeferred) xs = map p f xs >>| Seq.concat

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
          if e.MoveNext() then
            let v = createNode ()
            let rec loop x =
              if e.MoveNext() then
                let d = f x
                d >>> (fun y -> match y with Some _ -> d >-- v | None -> loop e.Current)
              else
                f x >-- v
            loop e.Current
            v :> _ IDeferred
          else
            value None
        ) (fun () -> e.Dispose(); unit)

    let tryFind (f : _ -> bool IDeferred) xs =
      xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))
