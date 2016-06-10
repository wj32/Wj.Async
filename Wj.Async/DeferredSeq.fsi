namespace Wj.Async

module DeferredSeq =
  type 'a T = 'a Next IDeferred
  and 'a Next =
    | Empty
    | Cons of head : 'a * tail : 'a T

  type 'a DSeq = 'a T

  // Writer

  module Writer =
    type 'a T

    val create : unit -> 'a T
    val isClosed : 'a T -> bool
    val close : 'a T -> unit
    val write : 'a T -> value : 'a -> unit
    val read : 'a T -> 'a DSeq

  val inline create : f : ('a Writer.T -> unit) -> 'a T
  val empty : unit -> 'a T

  // General

  val fold' : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a T -> 'state IDeferred
  val fold : folder : ('state -> 'a -> 'state) -> state : 'state -> s : 'a T -> 'state IDeferred
  val iter' : action : ('a -> unit IDeferred) -> s : 'a T -> unit IDeferred
  val iter : action : ('a -> unit) -> s : 'a T -> unit IDeferred
  val map' : mapping : ('a -> 'b IDeferred) -> s : 'a T -> 'b T
  val map : mapping : ('a -> 'b) -> s : 'a T -> 'b T
  val init : length : int -> initializer : (int -> 'a IDeferred) -> 'a T
  val collect : mapping : ('a -> 'b T) -> s : 'a T -> 'b T
  val choose : chooser : ('a -> 'b option IDeferred) -> s : 'a T -> 'b T
  val filter : predicate : ('a -> bool IDeferred) -> s : 'a T -> 'a T
  val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a T -> 'b option IDeferred
  val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a T -> 'a option IDeferred

  val first : s : 'a T -> 'a IDeferred
  val tryFirst : s : 'a T -> 'a option IDeferred
  val length : s : 'a T -> int IDeferred
  val concat : ss : 'a T T -> 'a T
  val append : s1 : 'a T -> s2 : 'a T -> 'a T
  val interleave : ss : 'a T T -> 'a T
  val take : count : int -> s : 'a T -> 'a T
  val takeDetermined : s : 'a T -> 'a seq * 'a T
  val takeUntil : event : unit IDeferred -> s : 'a T -> 'a T
  val unfold : generator : ('state -> ('a * 'state) option IDeferred) -> state : 'state -> 'a T

  val ofArray : 'a array -> 'a T
  val toArray : 'a T -> 'a array IDeferred
  val ofList : 'a list -> 'a T
  val toList : 'a T -> 'a list IDeferred
  val ofSeq : 'a seq -> 'a T
  val toSeq : 'a T -> 'a seq IDeferred
