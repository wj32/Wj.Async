namespace Wj.Async

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
    abstract member Close : unit -> unit
    abstract member WriteAccepted : unit IDeferred
    abstract member Write : value : 'a -> unit IDeferred
    abstract member WriteBatch : values : 'a array -> unit IDeferred
    abstract member WriteImmediately : value : 'a -> unit
    abstract member WriteBatchImmediately : values : 'a array -> unit

  [<Interface>]
  type 'a IReader =
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
  let inline writeAccepted (t : _ IWriter) = t.WriteAccepted
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
      mutable writeAccepted : unit IVar;
      pendingReads : 'a PendingRead.T Queue.T; }

    member inline t.IsClosedInternal = Deferred.isDetermined t.closed

    member t.CancelReads() =
      while not (Queue.isEmpty t.pendingReads) do
        match Queue.dequeue t.pendingReads with
        | PendingRead.Nothing v -> None --> v
        | PendingRead.Single v -> None --> v
        | PendingRead.Batch (b, v) -> None --> v

    member inline t.CloseInternal() =
      if not t.IsClosedInternal then
        () --> t.closed
        t.CancelReads()
        t.UpdateWriteAccepted()
        true
      else
        false

    member t.CompleteReads() =
      while not (Queue.isEmpty t.pendingReads || Queue.isEmpty t.buffer) do
        match Queue.dequeue t.pendingReads with
        | PendingRead.Nothing v -> Some () --> v
        | PendingRead.Single v -> Some (Queue.dequeue t.buffer) --> v
        | PendingRead.Batch (b, v) -> Some (Queue.dequeue' t.buffer (BatchSize.toInt b)) --> v

    member t.UpdateWriteAccepted() =
      if Queue.length t.buffer < t.capacity || t.IsClosedInternal then
        Deferred.trySet t.writeAccepted () |> ignore
      else
        if Deferred.isDetermined t.writeAccepted then
          t.writeAccepted <- Deferred.createVar ()

    member inline t.WriteImmediatelyInternal(f) =
      if t.IsClosedInternal then
        raise (new System.OperationCanceledException(PipeClosed))
      f ()
      t.CompleteReads()
      t.UpdateWriteAccepted()

    member inline t.ReadInternal(pendingRead, dequeue) =
      if Queue.isEmpty t.buffer then
        if t.IsClosedInternal then
          Deferred.value None
        else
          Deferred.create (fun v -> Queue.enqueue t.pendingReads (pendingRead v))
      else
        let result = Deferred.value (dequeue ())
        t.UpdateWriteAccepted()
        result

    member inline t.ReadImmediatelyInternal(dequeue) =
      if Queue.isEmpty t.buffer then
        None
      else
        let result = Some (dequeue ())
        t.UpdateWriteAccepted()
        result

    interface 'a IPipe with
      member t.Closed = t.closed :> _ IDeferred

      member t.IsClosed = t.IsClosedInternal

      member t.Length = Queue.length t.buffer

      member t.Capacity
        with get() = t.capacity
        and set(value) = t.capacity <- value

    interface 'a IWriter with
      member t.Close() = t.CloseInternal() |> ignore

      member t.WriteAccepted = t.writeAccepted :> _ IDeferred

      member t.Write(x) = (t :> _ IWriter).WriteImmediately(x); t.writeAccepted :> _ IDeferred

      member t.WriteBatch(xs) = (t :> _ IWriter).WriteBatchImmediately(xs); t.writeAccepted :> _ IDeferred

      member t.WriteImmediately(x) = t.WriteImmediatelyInternal(fun () -> Queue.enqueue t.buffer x)

      member t.WriteBatchImmediately(xs) = t.WriteImmediatelyInternal(fun () -> Queue.enqueue' t.buffer xs)

    interface 'a IReader with
      member t.CloseReader() =
        if Queue.isEmpty t.buffer then
          t.CloseInternal() |> ignore
        else
          Queue.clear t.buffer
          if not (t.CloseInternal()) then
            t.UpdateWriteAccepted()

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
        writeAccepted = Deferred.createVar ();
        pendingReads = Queue.create (); }
    () --> pipe.writeAccepted
    pipe

  let create () =
    let pipe = createPipe ()
    (pipe :> _ IWriter, pipe :> _ IReader)

  let inline createReader (f : _ -> unit IDeferred) =
    let (writer, reader) = create ()
    f writer >>> (fun () -> close writer)
    reader

  let inline createWriter (f : _ -> unit IDeferred) =
    let (writer, reader) = create ()
    f reader >>> (fun () -> closeReader reader)
    writer

  // Sequence processing

  let foldBatch b f state t = ()
  let fold' f state t = ()
  let fold f state t = ()
  let iterBatch b f t = ()
  let iter' f t = ()
  let iter f t = ()
  let mapBatch b f t = ()
  let map' f t = ()
  let map f t = ()
  let collect f t = ()
  let choose f t = ()
  let filter f t = ()

  // General

  let transferBatch b f writer reader = ()
  let transfer' f writer reader = ()
  let transfer f writer reader = ()
  let drain t = ()
  let concat ts = ()
  let append t1 t2 = ()
  let interleave ts = ()
  let interleavePipe tt = ()

  let ofArray xs = ()
  let toArray t = ()
  let ofList xs = ()
  let toList t = ()
  let ofSeq xs = ()
  let toSeq t = ()
