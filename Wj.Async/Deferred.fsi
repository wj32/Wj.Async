namespace Wj.Async

module Deferred =
  // IDeferred functions
  val upon : 'a IDeferred -> callback : ('a -> unit) -> unit
  val upon' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> unit
  val register : 'a IDeferred -> callback : ('a -> unit) -> IRegistration
  val register' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> IRegistration
  val moveFrom : 'a IDeferred -> from : 'a SupervisedCallback RegistrationList.T -> unit
  val isDetermined : 'a IDeferred -> bool
  val get : 'a IDeferred -> 'a
  val tryGet : 'a IDeferred -> 'a option
  // IVar functions
  val set : 'a IVar -> value : 'a -> unit
  val trySet : 'a IVar -> value : 'a -> bool
  val link : 'a IVar -> parent : 'a IDeferred -> unit
  val tryLink : 'a IVar -> parent : 'a IDeferred -> bool

  // Creation

  val value : value : 'a -> 'a IDeferred
  val createVar : unit -> 'a IVar
  val unit : unit IDeferred
  val never : unit -> 'a IDeferred

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
    val (>>=) : 'a IDeferred -> ('a -> 'b IDeferred) -> 'b IDeferred
    val (>>|) : 'a IDeferred -> ('a -> 'b) -> 'b IDeferred
    val (>>>) : 'a IDeferred -> ('a -> unit) -> unit
    val (-->) : 'a -> 'a IVar -> unit
    val (>--) : 'a IDeferred -> 'a IVar -> unit

  // General

  val allUnit : unit IDeferred list -> unit IDeferred
  val both : 'a IDeferred -> 'b IDeferred -> ('a * 'b) IDeferred
  val anyi : 'a IDeferred list -> (int * 'a) IDeferred
  val any : 'a IDeferred list -> 'a IDeferred
  val anyUnit : 'a IDeferred list -> unit IDeferred
  val dontWaitFor : unit IDeferred -> unit

  // Try-finally

  val tryFinally : f : (unit -> 'a IDeferred) -> finalizer : (unit -> unit IDeferred) -> 'a IDeferred

  // Conversion

  val ofAsync : 'a Async -> unit Async * 'a IDeferred
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
    type T<'state, 'a> = Repeat of 'state | Done of 'a

  val repeat : f : ('state -> Repeat.T<'state, 'a> IDeferred) -> state : 'state -> 'a IDeferred
  val repeatForever : f : ('state -> 'state IDeferred) -> state : 'state -> unit

  // Monad sequences

  module Array =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a array -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a array -> 'state IDeferred
    val all : s : 'a IDeferred array -> 'a array IDeferred
    val allUnit : s : unit IDeferred array -> unit IDeferred
    val iteri : parallelism : Parallelism.T -> action : (int -> 'a -> unit IDeferred) -> s : 'a array -> unit IDeferred
    val iter : parallelism : Parallelism.T -> action : ('a -> unit IDeferred) -> s : 'a array -> unit IDeferred
    val mapi : parallelism : Parallelism.T -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a array -> 'b array IDeferred
    val map : parallelism : Parallelism.T -> mapping : ('a -> 'b IDeferred) -> s : 'a array -> 'b array IDeferred
    val init : parallelism : Parallelism.T -> length : int -> initializer : (int -> 'a IDeferred) -> 'a array IDeferred
    val concatMap : parallelism : Parallelism.T -> mapping : ('a -> 'b array IDeferred) -> s : 'a array -> 'b array IDeferred
    val choose : parallelism : Parallelism.T -> chooser : ('a -> 'b option IDeferred) -> s : 'a array -> 'b array IDeferred
    val filter : parallelism : Parallelism.T -> predicate : ('a -> bool IDeferred) -> s : 'a array -> 'a array IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a array -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a array -> 'a option IDeferred

  module List =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a list -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a list -> 'state IDeferred
    val all : s : 'a IDeferred list -> 'a list IDeferred
    val allUnit : s : unit IDeferred list -> unit IDeferred
    val iteri : parallelism : Parallelism.T -> action : (int -> 'a -> unit IDeferred) -> s : 'a list -> unit IDeferred
    val iter : parallelism : Parallelism.T -> action : ('a -> unit IDeferred) -> s : 'a list -> unit IDeferred
    val mapi : parallelism : Parallelism.T -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a list -> 'b list IDeferred
    val map : parallelism : Parallelism.T -> mapping : ('a -> 'b IDeferred) -> s : 'a list -> 'b list IDeferred
    val init : parallelism : Parallelism.T -> length : int -> initializer : (int -> 'a IDeferred) -> 'a list IDeferred
    val concatMap : parallelism : Parallelism.T -> mapping : ('a -> 'b list IDeferred) -> s : 'a list -> 'b list IDeferred
    val choose : parallelism : Parallelism.T -> chooser : ('a -> 'b option IDeferred) -> s : 'a list -> 'b list IDeferred
    val filter : parallelism : Parallelism.T -> predicate : ('a -> bool IDeferred) -> s : 'a list -> 'a list IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a list -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a list -> 'a option IDeferred

  module Seq =
    val foldi : folder : (int -> 'state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a seq -> 'state IDeferred
    val fold : folder : ('state -> 'a -> 'state IDeferred) -> state : 'state -> s : 'a seq -> 'state IDeferred
    val all : s : 'a IDeferred seq -> 'a seq IDeferred
    val allUnit : s : unit IDeferred seq -> unit IDeferred
    val iteri : parallelism : Parallelism.T -> action : (int -> 'a -> unit IDeferred) -> s : 'a seq -> unit IDeferred
    val iter : parallelism : Parallelism.T -> action : ('a -> unit IDeferred) -> s : 'a seq -> unit IDeferred
    val mapi : parallelism : Parallelism.T -> mapping : (int -> 'a -> 'b IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val map : parallelism : Parallelism.T -> mapping : ('a -> 'b IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val init : parallelism : Parallelism.T -> length : int -> initializer : (int -> 'a IDeferred) -> 'a seq IDeferred
    val concatMap : parallelism : Parallelism.T -> mapping : ('a -> 'b seq IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val choose : parallelism : Parallelism.T -> chooser : ('a -> 'b option IDeferred) -> s : 'a seq -> 'b seq IDeferred
    val filter : parallelism : Parallelism.T -> predicate : ('a -> bool IDeferred) -> s : 'a seq -> 'a seq IDeferred
    val tryPick : chooser : ('a -> 'b option IDeferred) -> s : 'a seq -> 'b option IDeferred
    val tryFind : predicate : ('a -> bool IDeferred) -> s : 'a seq -> 'a option IDeferred
