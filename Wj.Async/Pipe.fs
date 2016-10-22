namespace Wj.Async

open Wj.Async.Internal

[<Interface>]
type 'a IPipe =
  abstract member Closed : unit IDeferred
  abstract member IsClosed : bool
  abstract member Length : int
  abstract member Capacity : int with get, set

module Pipe =
  open Deferred.Infix

  let [<Literal>] PipeClosed = "The pipe is closed."

  module BatchSize =
    type T =
      | Unlimited
      | AtMost of length : int

    let toInt t =
      match t with
      | Unlimited -> System.Int32.MaxValue
      | AtMost length -> length

  [<Interface>]
  type 'a IWriter =
    inherit ('a IPipe)

    abstract member Close : unit -> unit
    abstract member AcceptingWrite : unit IDeferred
    abstract member Write : value : 'a -> unit IDeferred
    abstract member WriteBatch : values : 'a array -> unit IDeferred
    abstract member WriteImmediately : value : 'a -> unit
    abstract member WriteBatchImmediately : values : 'a array -> unit

  [<Interface>]
  type 'a IReader =
    inherit ('a IPipe)

    abstract member CloseReader : unit -> unit
    abstract member Available : unit -> unit option IDeferred
    abstract member Read : unit -> 'a option IDeferred
    abstract member ReadBatch : batchSize : BatchSize.T -> 'a array option IDeferred
    abstract member ReadImmediately : unit -> 'a option
    abstract member ReadBatchImmediately : batchSize : BatchSize.T -> 'a array option

  // IPipe functions
  let inline closed (t : _ IPipe) = t.Closed
  let inline isClosed (t : _ IPipe) = t.IsClosed
  let inline length (t : _ IPipe) = t.Length
  let inline capacity (t : _ IPipe) = t.Capacity
  let inline setCapacity (t : _ IPipe) capacity = t.Capacity <- capacity
  // IWriter functions
  let inline close (t : _ IWriter) = t.Close()
  let inline acceptingWrite (t : _ IWriter) = t.AcceptingWrite
  let inline write (t : _ IWriter) x = t.Write(x)
  let inline writeBatch (t : _ IWriter) xs = t.WriteBatch(xs)
  let inline writeImmediately (t : _ IWriter) x = t.WriteImmediately(x)
  let inline writeBatchImmediately (t : _ IWriter) xs = t.WriteBatchImmediately(xs)
  // IReader functions
  let inline closeReader (t : _ IReader) = t.CloseReader()
  let inline available (t : _ IReader) = t.Available()
  let inline read (t : _ IReader) = t.Read()
  let inline readBatch (t : _ IReader) b = t.ReadBatch(b)
  let inline readImmediately (t : _ IReader) = t.ReadImmediately()
  let inline readBatchImmediately (t : _ IReader) b = t.ReadBatchImmediately(b)

  module PendingRead =
    type 'a T =
      | Nothing of result : unit option IVar
      | Single of result : 'a option IVar
      | Batch of batchSize : BatchSize.T * result : 'a array option IVar

  type T<'a> =
    { buffer : 'a Queue.T;
      closed : unit IVar;
      mutable capacity : int;
      mutable acceptingWrite : unit IVar;
      pendingReads : 'a PendingRead.T Queue.T; }

    member t.CancelReads() =
      while not (Queue.isEmpty t.pendingReads) do
        match Queue.dequeue t.pendingReads with
        | PendingRead.Nothing v -> v <-- None
        | PendingRead.Single v -> v <-- None
        | PendingRead.Batch (b, v) -> v <-- None

    member inline t.CloseInternal() =
      if not (isClosed t) then
        t.closed <-- ()
        t.CancelReads()
        t.UpdateAcceptingWrite()
        true
      else
        false

    member t.CompleteReads() =
      while not (Queue.isEmpty t.pendingReads || Queue.isEmpty t.buffer) do
        match Queue.dequeue t.pendingReads with
        | PendingRead.Nothing v -> v <-- Some ()
        | PendingRead.Single v -> v <-- Some (Queue.dequeue t.buffer)
        | PendingRead.Batch (b, v) -> v <-- Some (Queue.dequeue' t.buffer (BatchSize.toInt b))

    member t.UpdateAcceptingWrite() =
      if Queue.length t.buffer < t.capacity || isClosed t then
        Deferred.trySet t.acceptingWrite () |> ignore
      else
        if Deferred.isDetermined t.acceptingWrite then
          t.acceptingWrite <- Deferred.createVar ()

    member inline t.WriteImmediatelyInternal(f) =
      if isClosed t then
        raise (new System.OperationCanceledException(PipeClosed))
      f ()
      t.CompleteReads()
      t.UpdateAcceptingWrite()

    member inline t.ReadInternal(pendingRead, dequeue) =
      if Queue.isEmpty t.buffer then
        if isClosed t then
          Deferred.value None
        else
          Deferred.create (fun v -> Queue.enqueue t.pendingReads (pendingRead v))
      else
        let result = Deferred.value (dequeue ())
        t.UpdateAcceptingWrite()
        result

    member inline t.ReadImmediatelyInternal(dequeue) =
      if Queue.isEmpty t.buffer then
        None
      else
        let result = Some (dequeue ())
        t.UpdateAcceptingWrite()
        result

    interface 'a IPipe with
      member t.Closed = t.closed :> _ IDeferred

      member t.IsClosed = Deferred.isDetermined t.closed

      member t.Length = Queue.length t.buffer

      member t.Capacity
        with get() = t.capacity
        and set(value) = t.capacity <- value; t.UpdateAcceptingWrite()

    interface 'a IWriter with
      member t.Close() = t.CloseInternal() |> ignore

      member t.AcceptingWrite = t.acceptingWrite :> _ IDeferred

      member t.Write(x) = writeImmediately t x; t.acceptingWrite :> _ IDeferred

      member t.WriteBatch(xs) = writeBatchImmediately t xs; t.acceptingWrite :> _ IDeferred

      member t.WriteImmediately(x) = t.WriteImmediatelyInternal(fun () -> Queue.enqueue t.buffer x)

      member t.WriteBatchImmediately(xs) = t.WriteImmediatelyInternal(fun () -> Queue.enqueue' t.buffer xs)

    interface 'a IReader with
      member t.CloseReader() =
        if Queue.isEmpty t.buffer then
          t.CloseInternal() |> ignore
        else
          Queue.clear t.buffer
          if not (t.CloseInternal()) then
            t.UpdateAcceptingWrite()

      member t.Available() = t.ReadInternal(PendingRead.Nothing, fun () -> Some ())

      member t.Read() = t.ReadInternal(PendingRead.Single, fun () -> Some (Queue.dequeue t.buffer))

      member t.ReadBatch(b) =
        t.ReadInternal((fun v -> PendingRead.Batch (b, v)), fun () -> Some (Queue.dequeue' t.buffer (BatchSize.toInt b)))

      member t.ReadImmediately() = t.ReadImmediatelyInternal(fun () -> Queue.dequeue t.buffer)

      member t.ReadBatchImmediately(b) =
        t.ReadImmediatelyInternal(fun () -> Queue.dequeue' t.buffer (BatchSize.toInt b))

  let inline createPipe () =
    let pipe =
      { buffer = Queue.create ();
        closed = Deferred.createVar ();
        capacity = System.Int32.MaxValue;
        acceptingWrite = Deferred.createVar ();
        pendingReads = Queue.create (); }
    pipe.acceptingWrite <-- ()
    pipe

  let create () =
    let pipe = createPipe ()
    (pipe :> _ IWriter, pipe :> _ IReader)

  let inline createReader (f : _ -> unit IDeferred) =
    let writer, reader = create ()
    f writer >>> (fun () -> close writer)
    reader

  let inline createWriter (f : _ -> unit IDeferred) =
    let writer, reader = create ()
    f reader >>> (fun () -> closeReader reader)
    writer

  // Sequence processing

  let inline foldGeneric upon read f state =
    Deferred.create (fun v ->
      let rec loop state =
        read ()
        >>> function
        | Some x -> upon (f state x) loop
        | None -> v <-- state
      Deferred.unit >>> fun () -> loop state
    )

  let inline foldBatchInline b (f : _ -> _ -> _ IDeferred) state t =
    foldGeneric Deferred.upon (fun () -> readBatch t b) f state

  let foldBatch b f state t = foldBatchInline b f state t

  let inline foldInline' (f : _ -> _ -> _ IDeferred) state t =
    foldGeneric Deferred.upon (fun () -> read t) f state

  let fold' f state t = foldInline' f state t

  let inline foldInline f state t = foldGeneric (|>) (fun () -> read t) f state

  let fold f state t = foldInline f state t

  let iterBatch b f t = foldBatchInline b (fun () xs -> f xs) () t

  let iter' f t = foldInline' (fun () x -> f x) () t

  let inline iterInline f t = foldInline (fun () x -> f x) () t

  let iter f t = iterInline f t

  let inline createIterGeneric readImmediately f writer reader =
    Deferred.create (fun v ->
      let inline complete () = v <-- ()
      let inline closeReaderAndComplete () = closeReader reader; complete ()
      let rec loop () =
        if isClosed writer then
          closeReaderAndComplete ()
        else
          match readImmediately () with
          | Some x -> f x >>> loop
          | None ->
            if isClosed reader then
              complete ()
            else
              Deferred.choose
                [ Deferred.choice (available reader) ignore;
                  Deferred.choice (closed writer) ignore; ]
              >>> loop
      Deferred.unit >>> loop
    )

  let inline seqToArrayReadOnly (s : _ seq) =
    match s with
    | :? (_ array) as s -> s
    | _ -> Seq.toArray s

  let mapBatch b (f : _ -> _ IDeferred) t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readBatchImmediately t b) (fun xs ->
        f xs >>= fun ys -> writeBatch writer (seqToArrayReadOnly ys)
      ) writer t
    )

  let map' (f : _ -> _ IDeferred) t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readImmediately t) (fun x -> f x >>= write writer) writer t
    )

  let map f t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readImmediately t) (f >> write writer) writer t
    )

  let collect (f : _ -> _ IDeferred) t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readImmediately t) (fun x ->
        f x >>= (fun ys -> writeBatch writer (seqToArrayReadOnly ys))
      ) writer t
    )

  let choose (f : _ -> _ IDeferred) t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readImmediately t) (fun x ->
        f x
        >>= function
        | Some y -> write writer y
        | None -> Deferred.unit
      ) writer t
    )

  let filter (f : _ -> _ IDeferred) t =
    createReader (fun writer ->
      createIterGeneric (fun () -> readImmediately t) (fun x ->
        f x
        >>= function
        | true -> write writer x
        | false -> Deferred.unit
      ) writer t
    )

  // General

  let transferBatch b (f : _ -> _ IDeferred) writer reader =
    createIterGeneric (fun () -> readBatchImmediately reader b) (fun xs ->
      f xs >>= fun ys -> writeBatch writer (seqToArrayReadOnly ys)
    ) writer reader

  let transfer' (f : _ -> _ IDeferred) writer reader =
    createIterGeneric (fun () -> readImmediately reader) (fun x -> f x >>= write writer) writer reader

  let transfer f writer reader =
    createIterGeneric (fun () -> readImmediately reader) (f >> write writer) writer reader

  let transferId writer reader =
    createIterGeneric (fun () -> readImmediately reader) (write writer) writer reader

  let inline readAllSystemList cast t =
    let list = new System.Collections.Generic.List<_>()
    let result = cast list
    let resultNow = Deferred.value result
    foldBatchInline BatchSize.Unlimited (fun _ xs -> list.AddRange(xs); resultNow) result t

  let readAll t = readAllSystemList id t >>| fun list -> list.ToArray()

  let drain t = foldBatchInline BatchSize.Unlimited (fun n xs -> Deferred.value (n + xs.Length)) 0 t

  let concat (ts : _ IReader list) =
    createReader (fun writer -> Deferred.List.iter Parallelism.sequential (transferId writer) ts)

  let append t1 t2 = createReader (fun writer -> transferId writer t1 >>= fun () -> transferId writer t2)

  let inline interleaveGeneric upon iter ts =
    let writer, reader = create ()
    let mutable active = 1
    let inline increment () = active <- active + 1
    let inline decrement () = active <- active - 1; if active = 0 then close writer
    upon (iter (fun t ->
      increment ()
      transferId writer t >>> decrement
    ) ts) decrement
    reader

  let interleave (ts : _ IReader list) = interleaveGeneric (|>) List.iter ts

  let interleavePipe (tt : _ IReader IReader) = interleaveGeneric Deferred.upon iterInline tt

  let choice t = Deferred.choice (available t) (function Some () -> readImmediately t | None -> None)

  let ofArray xs = createReader (fun writer -> writeBatch writer xs)

  let toArray t = readAllSystemList id t >>| fun list -> list.ToArray()

  let ofList xs = createReader (fun writer -> writeBatch writer (List.toArray xs))

  let toList t = readAllSystemList id t >>| Seq.toList

  let ofSeq xs = createReader (fun writer -> writeBatch writer (Seq.toArray xs))

  let toSeq t = readAllSystemList (fun list -> list :> _ seq) t
