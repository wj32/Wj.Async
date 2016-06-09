namespace Wj.Async

module DeferredSeq =
  type 'a T = 'a Next IDeferred
  and 'a Next =
    | Empty
    | Cons of head : 'a * tail : 'a T

  // Writer

  module Writer =
    type 'a T

    val isClosed : 'a T -> bool
    val close : 'a T -> unit
    val write : 'a T -> value : 'a -> unit

  val create : unit -> 'a T * 'a Writer.T

  // General

  val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a T -> 'state IDeferred
  val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a T -> 'state IDeferred
  val iteri : action : (int -> 'a -> unit IDeferred) -> s : 'a T -> unit IDeferred
  val iter : action : ('a -> unit IDeferred) -> s : 'a T -> unit IDeferred
  val mapi : mapping : (int -> 'a -> 'b IDeferred) -> s : 'a T -> 'b T
  val map : mapping : ('a -> 'b IDeferred) -> s : 'a T -> 'b T
  val init : length : int -> initializer : (int -> 'a IDeferred) -> 'a T
  val concatMap : mapping : ('a -> 'b T) -> s : 'a T -> 'b T
  val choose : chooser : ('a -> 'b option IDeferred) -> s : 'a T -> 'b T IDeferred
  val filter : predicate : ('a -> bool IDeferred) -> s : 'a T -> 'a T IDeferred
  val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a T -> 'b option IDeferred
  val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a T -> 'a option IDeferred

  val first : 'a T -> 'a IDeferred
  val tryFirst : 'a T -> 'a option IDeferred
  val concat : 'a T T -> 'a T
  val append : s1 : 'a T -> s2 : 'a T -> 'a T

  val ofArray : 'a array -> 'a T
  val toArray : 'a T -> 'a array IDeferred
  val ofList : 'a list -> 'a T
  val toList : 'a T -> 'a list IDeferred
  val ofSeq : 'a seq -> 'a T
  val toSeq : 'a T -> 'a seq IDeferred
