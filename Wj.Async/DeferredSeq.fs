﻿namespace Wj.Async

module DeferredSeq =
  open Deferred.Infix

  let [<Literal>] DeferredSeqEmpty = "The sequence is empty."

  type 'a T = 'a Next IDeferred
  and 'a Next =
    | Empty
    | Cons of head : 'a * tail : 'a T

  type 'a DSeq = 'a T

  type 'a Unique() =
    static member empty = Deferred.value (Empty : 'a Next)

  module Writer =
    let [<Literal>] WriterAlreadyClosed = "The writer is already closed."

    type 'a T = {mutable next : 'a Next IVar}

    let create () = {next = Deferred.createVar ()}

    let isClosed t = Deferred.isDetermined t.next

    let close t = if not (Deferred.trySet t.next Empty) then invalidOp WriterAlreadyClosed

    let write t x =
      let next' = Deferred.createVar ()
      if not (Deferred.trySet t.next (Cons (x, next'))) then
        invalidOp WriterAlreadyClosed
      t.next <- next'

    let read t = t.next :> _ IDeferred

  let inline create f =
    let writer = Writer.create ()
    let t = Writer.read writer
    f writer
    t

  let inline create' (f : _ -> unit IDeferred) =
    let writer = Writer.create ()
    let t = Writer.read writer
    f writer >>> (fun () -> Writer.close writer)
    t

  let empty () = Unique.empty

  let inline foldGeneric upon f state xs =
    Deferred.create (fun v ->
      let rec loop state tail =
        tail
        >>> function
        | Empty -> v <-- state
        | Cons (head, tail) -> upon (f state head) (fun state -> loop state tail)
      loop state xs
    )

  let inline foldInline' (f : _ -> _ -> _ IDeferred) state xs = foldGeneric Deferred.upon f state xs

  let fold' f state xs = foldInline' f state xs

  let inline foldInline f state xs = foldGeneric (|>) f state xs

  let fold f state xs = foldInline f state xs

  let inline iterInline' f xs = xs |> foldInline' (fun () x -> f x) ()

  let iter' f xs = iterInline' f xs

  let inline iterInline f xs = xs |> foldInline (fun () x -> f x) ()

  let iter f xs = iterInline f xs

  let inline createIterGeneric iterSpecific f xs =
    create' (fun writer -> xs |> iterSpecific (fun x -> f (Writer.write writer) x))

  let inline createIter' (f : _ -> _ -> unit IDeferred) xs = createIterGeneric iterInline' f xs

  let inline createIter (f : _ -> _ -> unit) xs = createIterGeneric iterInline f xs

  let map' (f : _ -> _ IDeferred) xs = xs |> createIter' (fun write x -> f x >>| (fun y -> write y))

  let map f xs = xs |> createIter (fun write x -> f x |> (fun y -> write y))

  let init length (f : _ -> _ IDeferred) =
    create (fun writer ->
      let rec loop i =
        if i = length then
          Writer.close writer
        else
          f i >>> (fun x -> Writer.write writer x; loop (i + 1))
      loop 0
    )

  let collect (f : _ -> _ T) xs =
    xs |> createIter' (fun write x -> f x |> iterInline (fun y -> write y))

  let choose (f : _ -> _ IDeferred) xs =
    xs |> createIter' (fun write x -> f x >>| (function Some y -> write y | None -> ()))

  let filter (f : _ -> _ IDeferred) xs =
    xs |> createIter' (fun write x -> f x >>| (fun b -> if b then write x))

  let tryPick (f : _ -> _ IDeferred) xs =
    Deferred.create (fun v ->
      let rec loop tail =
        tail
        >>> function
        | Empty -> v <-- None
        | Cons (head, tail) -> f head >>> (fun y -> match y with Some _ -> v <-- y | None -> loop tail)
      loop xs
    )

  let tryFind (f : _ -> bool IDeferred) xs =
    xs |> tryPick (fun x -> f x >>| (fun b -> if b then Some x else None))

  let first xs = xs >>| (function Empty -> invalidOp DeferredSeqEmpty | Cons (head, _) -> head)

  let tryFirst xs = xs >>| (function Empty -> None | Cons (head, _) -> Some head)

  let length xs = xs |> foldInline (fun n x -> n + 1) 0

  let concat (xss : _ T T) = xss |> createIter' (fun write xs -> xs |> iterInline (fun x -> write x))

  let append xs1 xs2 =
    create (fun writer ->
      xs1 |> iterInline (Writer.write writer)
      >>> fun () ->
      xs2 |> iterInline (Writer.write writer)
      >>> fun () ->
      Writer.close writer
    )

  let interleave (xss : _ T T) =
    create (fun writer ->
      let mutable active = 1
      let inline increment () = active <- active + 1
      let inline decrement () = active <- active - 1; if active = 0 then Writer.close writer
      xss |> iterInline (fun xs ->
        increment ()
        xs |> iterInline (Writer.write writer) >>> decrement
      ) >>> decrement
    )

  let take count xs =
    create (fun writer ->
      let rec loop n tail =
        if n = 0 then
          Writer.close writer
        else
          tail
          >>> function
          | Empty -> Writer.close writer
          | Cons (head, tail) -> Writer.write writer head; loop (n - 1) tail
      loop count xs
    )

  let takeDetermined xs =
    let list = new System.Collections.Generic.List<_>()
    let rec loop tail =
      match Deferred.tryGet tail with
      | Some (Cons (head, tail)) -> list.Add(head); loop tail
      | _ -> (list.ToArray(), tail)
    loop xs

  let takeUntil event xs =
    create (fun writer ->
      let rec loop tail =
        Deferred.choose [Deferred.choice event (fun () -> Empty); Deferred.choice tail id]
        >>> function
        | Empty -> Writer.close writer
        | Cons (head, tail) -> Writer.write writer head; loop tail
      loop xs
    )

  let unfold (f : _ -> _ IDeferred) state =
    create (fun writer ->
      let rec loop state =
        f state
        >>> function
        | Some (x, state) -> Writer.write writer x; loop state
        | None -> Writer.close writer
      loop state
    )

  let toSystemList cast xs =
    let list = new System.Collections.Generic.List<_>()
    xs |> foldInline (fun l x -> list.Add(x); l) (cast list)

  let ofArray (xs : _ array) =
    create (fun writer -> for x in xs do Writer.write writer x; Writer.close writer)

  let toArray xs = toSystemList id xs >>| (fun list -> list.ToArray())

  let ofList (xs : _ list) =
    create (fun writer -> for x in xs do Writer.write writer x; Writer.close writer)

  let toList xs = xs |> foldInline (fun acc x -> x :: acc) [] >>| List.rev

  let ofSeq xs = create (fun writer -> for x in xs do Writer.write writer x; Writer.close writer)

  let toSeq xs =
    xs |> iterInline (fun x -> ())
    >>| fun () -> seq {
      let mutable tail = xs
      while Deferred.isDetermined tail do
        match Deferred.get tail with
        | Empty -> tail <- Deferred.never ()
        | Cons (head, tail') -> yield head; tail <- tail'
    }
