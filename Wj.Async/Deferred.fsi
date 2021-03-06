﻿namespace Wj.Async

module Deferred =
  // IDeferred functions
  val inline upon : 'a IDeferred -> callback : ('a -> unit) -> unit
  val inline upon' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> unit
  val inline register : 'a IDeferred -> callback : ('a -> unit) -> IRegistration
  val inline register' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> IRegistration
  val inline moveFrom : 'a IDeferred -> from : 'a SupervisedCallback RegistrationList.T -> unit
  val inline isDetermined : 'a IDeferred -> bool
  val inline get : 'a IDeferred -> 'a
  val inline tryGet : 'a IDeferred -> 'a option
  // IVar functions
  val inline set : 'a IVar -> value : 'a -> unit
  val inline trySet : 'a IVar -> value : 'a -> bool
  val inline link : 'a IVar -> parent : 'a IDeferred -> unit
  val inline tryLink : 'a IVar -> parent : 'a IDeferred -> bool

  // Creation

  val value : value : 'a -> 'a IDeferred
  val createVar : unit -> 'a IVar
  val inline create : f : ('a IVar -> unit) -> 'a IDeferred
  val unit : unit IDeferred
  val never : unit -> 'a IDeferred

  val start : f : (unit -> 'a) -> 'a IDeferred
  val startThread : threadType : ThreadType.T -> f : (unit -> 'a) -> 'a IDeferred

  // Monad

  val ``return`` : 'a -> 'a IDeferred // Same as value
  val bind : 'a IDeferred -> ('a -> 'b IDeferred) -> 'b IDeferred
  val map : 'a IDeferred -> ('a -> 'b) -> 'b IDeferred
  val join : 'a IDeferred IDeferred -> 'a IDeferred
  val forget : 'a IDeferred -> unit IDeferred
  val all : 'a IDeferred list -> 'a list IDeferred
  val allForget : unit IDeferred list -> unit IDeferred

  // Infix operators

  module Infix =
    val inline (>>=) : 'a IDeferred -> ('a -> 'b IDeferred) -> 'b IDeferred
    val inline (>>|) : 'a IDeferred -> ('a -> 'b) -> 'b IDeferred
    val inline (>>>) : 'a IDeferred -> ('a -> unit) -> unit
    val inline (<--) : 'a IVar -> 'a -> unit
    val inline (--<) : 'a IVar -> 'a IDeferred -> unit

  // General

  val allUnit : unit IDeferred list -> unit IDeferred
  val both : 'a IDeferred -> 'b IDeferred -> ('a * 'b) IDeferred
  val anyi : 'a IDeferred list -> (int * 'a) IDeferred
  val any : 'a IDeferred list -> 'a IDeferred
  val anyUnit : 'a IDeferred list -> unit IDeferred
  val inline dontWaitFor : unit IDeferred -> unit

  // Try-finally

  val tryFinally : f : (unit -> 'a IDeferred) -> finalizer : (unit -> unit IDeferred) -> 'a IDeferred

  // Conversion

  val ofAsyncStart : 'a Async -> 'a IDeferred
  val inline internal ofBeginEnd
    : ``begin`` : (System.AsyncCallback -> 'unused)
    -> ``end`` : (System.IAsyncResult -> 'a)
    -> 'a IDeferred
  val ofTask : 'a System.Threading.Tasks.Task -> 'a IDeferred
  val ofTaskUnit : System.Threading.Tasks.Task -> unit IDeferred

  // Choice

  [<Interface>]
  type 'b IChoice =
    abstract member Register : supervisedCallback : unit SupervisedCallback -> IRegistration
    abstract member TryGetApply : unit -> 'b option

  val choice : 'a IDeferred -> f : ('a -> 'b) -> 'b IChoice
  val choose : 'b IChoice list -> 'b IDeferred

  // Repeat

  module Repeat =
    [<ReferenceEquality>]
    type T<'state, 'a> = Repeat of 'state | Done of 'a

  val repeat : f : ('state -> Repeat.T<'state, 'a> IDeferred) -> state : 'state -> 'a IDeferred
  val repeatForever : f : ('state -> 'state IDeferred) -> state : 'state -> unit

  // Concurrency

  type ConcurrentConcurrency =
    | Unique

    interface IConcurrency

  type SequentialConcurrency =
    new : unit -> SequentialConcurrency

    interface IConcurrency

  [<Class>]
  type LocallySequentialConcurrency =
    inherit SequentialConcurrency

    static member Unique : LocallySequentialConcurrency

  val concurrently
    : concurrency : IConcurrency
    -> f : ('a -> 'b IDeferred)
    -> ('a -> 'b IDeferred)
  val concurrently2
    : concurrency : IConcurrency
    -> f : ('a -> 'b -> 'c IDeferred)
    -> ('a -> 'b -> 'c IDeferred)

  // Monad sequences

  module Array =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a array -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a array -> 'state IDeferred
    val iteri : concurrency : IConcurrency -> action : (int -> 'a -> unit IDeferred) -> s : 'a array -> unit IDeferred
    val iter : concurrency : IConcurrency -> action : ('a -> unit IDeferred) -> s : 'a array -> unit IDeferred
    val mapi : concurrency : IConcurrency -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a array -> 'b array IDeferred
    val map : concurrency : IConcurrency -> mapping : ('a -> 'b IDeferred) -> s : 'a array -> 'b array IDeferred
    val init : concurrency : IConcurrency -> length : int -> initializer : (int -> 'a IDeferred) -> 'a array IDeferred
    val collect : concurrency : IConcurrency -> mapping : ('a -> 'b array IDeferred) -> s : 'a array -> 'b array IDeferred
    val choose : concurrency : IConcurrency -> chooser : ('a -> 'b option IDeferred) -> s : 'a array -> 'b array IDeferred
    val filter : concurrency : IConcurrency -> predicate : ('a -> bool IDeferred) -> s : 'a array -> 'a array IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a array -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a array -> 'a option IDeferred
    val all : s : 'a IDeferred array -> 'a array IDeferred
    val allUnit : s : unit IDeferred array -> unit IDeferred

  module List =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a list -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a list -> 'state IDeferred
    val iteri : concurrency : IConcurrency -> action : (int -> 'a -> unit IDeferred) -> s : 'a list -> unit IDeferred
    val iter : concurrency : IConcurrency -> action : ('a -> unit IDeferred) -> s : 'a list -> unit IDeferred
    val mapi : concurrency : IConcurrency -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a list -> 'b list IDeferred
    val map : concurrency : IConcurrency -> mapping : ('a -> 'b IDeferred) -> s : 'a list -> 'b list IDeferred
    val init : concurrency : IConcurrency -> length : int -> initializer : (int -> 'a IDeferred) -> 'a list IDeferred
    val collect : concurrency : IConcurrency -> mapping : ('a -> 'b list IDeferred) -> s : 'a list -> 'b list IDeferred
    val choose : concurrency : IConcurrency -> chooser : ('a -> 'b option IDeferred) -> s : 'a list -> 'b list IDeferred
    val filter : concurrency : IConcurrency -> predicate : ('a -> bool IDeferred) -> s : 'a list -> 'a list IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a list -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a list -> 'a option IDeferred
    val all : s : 'a IDeferred list -> 'a list IDeferred
    val allUnit : s : unit IDeferred list -> unit IDeferred

  module Seq =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a seq -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a seq -> 'state IDeferred
    val iteri : concurrency : IConcurrency -> action : (int -> 'a -> unit IDeferred) -> s : 'a seq -> unit IDeferred
    val iter : concurrency : IConcurrency -> action : ('a -> unit IDeferred) -> s : 'a seq -> unit IDeferred
    val mapi : concurrency : IConcurrency -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val map : concurrency : IConcurrency -> mapping : ('a -> 'b IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val init : concurrency : IConcurrency -> length : int -> initializer : (int -> 'a IDeferred) -> 'a seq IDeferred
    val collect : concurrency : IConcurrency -> mapping : ('a -> IDeferred<#seq<'b>>) -> s : 'a seq -> 'b seq IDeferred
    val choose : concurrency : IConcurrency -> chooser : ('a -> 'b option IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val filter : concurrency : IConcurrency -> predicate : ('a -> bool IDeferred) -> s : 'a seq -> 'a seq IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a seq -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a seq -> 'a option IDeferred
    val all : s : 'a IDeferred seq -> 'a seq IDeferred
    val allUnit : s : unit IDeferred seq -> unit IDeferred
