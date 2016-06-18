namespace Wj.Async

[<Interface>]
type 'a IPipe =
  abstract member Closed : unit IDeferred
  abstract member IsClosed : bool
  abstract member Length : int
  abstract member Capacity : int with get, set

module Pipe =
  module BatchSize =
    type T =
      | Unlimited
      | AtMost of length : int

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
  val inline closed : 'a IPipe -> unit IDeferred
  val inline isClosed : 'a IPipe -> bool
  val inline length : 'a IPipe -> int
  val inline capacity : 'a IPipe -> int
  val inline setCapacity : 'a IPipe -> capacity : int -> unit
  // IWriter functions
  val inline close : 'a IWriter -> unit
  val inline acceptingWrite : 'a IWriter -> unit IDeferred
  val inline write : 'a IWriter -> value : 'a -> unit IDeferred
  val inline writeBatch : 'a IWriter -> values : 'a array -> unit IDeferred
  val inline writeImmediately : 'a IWriter -> value : 'a -> unit
  val inline writeBatchImmediately : 'a IWriter -> values : 'a array -> unit
  // IReader functions
  val inline closeReader : 'a IReader -> unit
  val inline available : 'a IReader -> unit option IDeferred
  val inline read : 'a IReader -> 'a option IDeferred
  val inline readBatch : 'a IReader -> batchSize : BatchSize.T -> 'a array option IDeferred
  val inline readImmediately : 'a IReader -> 'a option
  val inline readBatchImmediately : 'a IReader -> batchSize : BatchSize.T -> 'a array option

  // Creation

  val create : unit -> 'a IWriter * 'a IReader
  val inline createReader : f : ('a IWriter -> unit IDeferred) -> 'a IReader
  val inline createWriter : f : ('a IReader -> unit IDeferred) -> 'a IWriter

  // Sequence processing

  val foldBatch : batchSize : BatchSize.T -> folder : ('state -> 'a array -> 'state IDeferred) -> state : 'state -> reader : 'a IReader -> 'state IDeferred
  val fold' : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> reader : 'a IReader -> 'state IDeferred
  val fold : folder : ('state -> 'a -> 'state) -> state : 'state -> reader : 'a IReader -> 'state IDeferred
  val iterBatch : batchSize : BatchSize.T -> action : ('a array -> unit IDeferred) -> reader : 'a IReader -> unit IDeferred
  val iter' : action : ('a -> unit IDeferred) -> reader : 'a IReader -> unit IDeferred
  val iter : action : ('a -> unit) -> reader : 'a IReader -> unit IDeferred
  val mapBatch : batchSize : BatchSize.T -> mapping : ('a array -> IDeferred<#seq<'b>>) -> reader : 'a IReader -> 'b IReader
  val map' : mapping : ('a -> 'b IDeferred) -> reader : 'a IReader -> 'b IReader
  val map : mapping : ('a -> 'b) -> reader : 'a IReader -> 'b IReader
  val collect : mapping : ('a -> IDeferred<#seq<'b>>) -> reader : 'a IReader -> 'b IReader
  val choose : chooser : ('a -> 'b option IDeferred) -> reader : 'a IReader -> 'b IReader
  val filter : predicate : ('a -> bool IDeferred) -> reader : 'a IReader -> 'a IReader

  // General

  val transferBatch : batchSize : BatchSize.T -> mapping : ('a array -> IDeferred<#seq<'b>>) -> writer : 'b IWriter -> reader : 'a IReader -> unit IDeferred
  val transfer' : mapping : ('a -> 'b IDeferred) -> writer : 'b IWriter -> reader : 'a IReader -> unit IDeferred
  val transfer : mapping : ('a -> 'b) -> writer : 'b IWriter -> reader : 'a IReader -> unit IDeferred
  val transferId : writer : 'a IWriter -> reader : 'a IReader -> unit IDeferred
  val readAll : reader : 'a IReader -> 'a array IDeferred
  val drain : reader : 'a IReader -> int IDeferred
  val concat : readers : 'a IReader list -> 'a IReader
  val append : reader1 : 'a IReader -> reader2 : 'a IReader -> 'a IReader
  val interleave : readers : 'a IReader list -> 'a IReader
  val interleavePipe : readers : 'a IReader IReader -> 'a IReader

  val ofArray : 'a array -> 'a IReader
  val toArray : 'a IReader -> 'a array IDeferred
  val ofList : 'a list -> 'a IReader
  val toList : 'a IReader -> 'a list IDeferred
  val ofSeq : 'a seq -> 'a IReader
  val toSeq : 'a IReader -> 'a seq IDeferred
