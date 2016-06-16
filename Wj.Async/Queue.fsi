namespace Wj.Async.Internal

module Queue =
  type 'a T

  val create : unit -> 'a T
  val createWithCapacity : capacity : int -> 'a T

  val enqueue : 'a T -> value : 'a -> unit
  val enqueue' : 'a T -> values : 'a array -> unit
  val dequeue : 'a T -> 'a
  val dequeue' : 'a T -> maxLength : int -> 'a array
  val tryDequeue : 'a T -> 'a option
  val clear : 'a T -> unit

  val first : 'a T -> 'a
  val tryFirst : 'a T -> 'a option
  val last : 'a T -> 'a
  val tryLast : 'a T -> 'a option
  val length : 'a T -> int
  val isEmpty : 'a T -> bool

  val ofArray : 'a array -> 'a T
  val toArray : 'a T -> 'a array
  val ofList : 'a list -> 'a T
  val toList : 'a T -> 'a list
  val ofSeq : 'a seq -> 'a T
  val toSeq : 'a T -> 'a seq

  // Sequence processing

  val fold : folder : ('state -> 'a -> 'state) -> state : 'state -> queue : 'a T -> 'state
  val iter : action : ('a -> unit) -> queue : 'a T -> unit
  val map : mapping : ('a -> 'b) -> queue : 'a T -> 'b T
  val choose : chooser : ('a -> 'b option) -> queue : 'a T -> 'b T
  val filter : predicate : ('a -> bool) -> queue : 'a T -> 'a T
