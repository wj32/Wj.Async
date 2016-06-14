namespace Wj.Async

module Pipe =
  type T<'a, 'phantom>
  type Pipe<'a, 'phantom> = T<'a, 'phantom>

  module Writer =
    type Phantom
    type 'a T = Pipe<'a, Phantom>

  module Reader =
    type Phantom
    type 'a T = Pipe<'a, Phantom>

  val create : unit -> 'a Writer.T * 'a Reader.T
  val createReader : f : ('a Writer.T -> unit IDeferred) -> 'a Reader.T
  val createWriter : f : ('a Reader.T -> unit IDeferred) -> 'a Writer.T
  val close : writer : 'a Writer.T -> unit
  val closeReader : reader : 'a Reader.T -> unit

  // General information

  val isClosed : T<'a, 'phantom> -> bool
  val length : T<'a, 'phantom> -> int
  val capacity : T<'a, 'phantom> -> int
  val setCapacity : T<'a, 'phantom> -> int -> unit

  // Writer

  val withinCapacity : writer : 'a Writer.T -> unit IDeferred
  val writeBatch : writer : 'a Writer.T -> values : 'a seq -> unit IDeferred
  val write : writer : 'a Writer.T -> value : 'a -> unit IDeferred
  val writeBatchIgnoreCapacity : writer : 'a Writer.T -> values : 'a seq -> unit
  val writeIgnoreCapacity : writer : 'a Writer.T -> value : 'a -> unit

  // Reader

  module BatchSize =
    type T =
      | Unlimited
      | AtMost of length : int

  val readBatch : reader : 'a Reader.T -> batchSize : BatchSize.T -> 'a array option IDeferred
  val read : reader : 'a Reader.T -> 'a option IDeferred

  // Sequence processing

  val foldBatch : batchSize : BatchSize.T -> folder : ('state -> 'a array -> 'state IDeferred) -> state : 'state -> reader : 'a Reader.T -> 'state IDeferred
  val fold' : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> reader : 'a Reader.T -> 'state IDeferred
  val fold : folder : ('state -> 'a -> 'state) -> state : 'state -> reader : 'a Reader.T -> 'state IDeferred
  val iterBatch : batchSize : BatchSize.T -> action : ('a array -> unit IDeferred) -> reader : 'a Reader.T -> unit IDeferred
  val iter' : action : ('a -> unit IDeferred) -> reader : 'a Reader.T -> unit IDeferred
  val iter : action : ('a -> unit) -> reader : 'a Reader.T -> unit IDeferred
  val mapBatch : batchSize : BatchSize.T -> mapping : ('a array -> IDeferred<#seq<'b>>) -> reader : 'a Reader.T -> 'b Reader.T
  val map' : mapping : ('a -> 'b IDeferred) -> reader : 'a Reader.T -> 'b Reader.T
  val map : mapping : ('a -> 'b) -> reader : 'a Reader.T -> 'b Reader.T
  val collect : mapping : ('a -> IDeferred<#seq<'b>>) -> reader : 'a Reader.T -> 'b Reader.T
  val choose : chooser : ('a -> 'b option IDeferred) -> reader : 'a Reader.T -> 'b Reader.T
  val filter : predicate : ('a -> bool IDeferred) -> reader : 'a Reader.T -> 'a Reader.T

  // General

  val transferBatch : batchSize : BatchSize.T -> mapping : ('a array -> IDeferred<#seq<'b>>) -> writer : 'b Writer.T -> reader : 'a Reader.T -> unit IDeferred
  val transfer' : mapping : ('a -> 'b IDeferred) -> writer : 'b Writer.T -> reader : 'a Reader.T -> unit IDeferred
  val transfer : mapping : ('a -> 'b) -> writer : 'b Writer.T -> reader : 'a Reader.T -> unit IDeferred
  val drain : reader : 'a Reader.T -> int IDeferred
  val concat : readers : 'a Reader.T list -> 'a Reader.T
  val append : reader1 : 'a Reader.T -> reader2 : 'a Reader.T -> 'a Reader.T
  val interleave : readers : 'a Reader.T list -> 'a Reader.T
  val interleavePipe : readers : 'a Reader.T Reader.T -> 'a Reader.T

  val ofArray : 'a array -> 'a Reader.T
  val ofList : 'a list -> 'a Reader.T
  val ofSeq : 'a seq -> 'a Reader.T
