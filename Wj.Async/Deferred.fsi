namespace Wj.Async

open System.Threading.Tasks;

module Deferred =
  // IDeferred functions
  val upon : 'a IDeferred -> callback : ('a -> unit) -> unit
  val upon' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> unit
  val register : 'a IDeferred -> callback : ('a -> unit) -> IRegistration
  val register' : 'a IDeferred -> supervisedCallback : 'a SupervisedCallback -> IRegistration
  val moveFrom : 'a IDeferred -> from : 'a SupervisedCallback RegistrationList.T -> unit
  val get : 'a IDeferred -> 'a
  val tryGet : 'a IDeferred -> 'a option
  val isDetermined : 'a IDeferred -> bool
  // IVar functions
  val set : 'a IVar -> value : 'a -> unit
  val trySet : 'a IVar -> value : 'a -> bool
  // INode functions
  val isLinked : 'a INode -> bool
  val link : 'a INode -> parent : 'a IDeferred -> unit
  val tryLink : 'a INode -> parent : 'a IDeferred -> bool

  val create : value : 'a -> 'a IDeferred
  val createVar : unit -> 'a IVar
  val createNode : unit -> 'a INode
  val unit : unit IDeferred
  val never : unit -> 'a IDeferred

  // Monad
  val ``return`` : value : 'a -> 'a IDeferred // Same as create
  val bind : 'a IDeferred -> f : ('a -> 'b IDeferred) -> 'b IDeferred

  // Standard monad functions
  val map : 'a IDeferred -> f : ('a -> 'b) -> 'b IDeferred
  val join : 'a IDeferred IDeferred -> 'a IDeferred
  val forget : 'a IDeferred -> unit IDeferred
  val all : 'a IDeferred list -> 'a list IDeferred
  val allForget : unit IDeferred list -> unit IDeferred

  // Extra convenience functions
  val both : 'a IDeferred -> 'b IDeferred -> ('a * 'b) IDeferred
  val allUnit : unit IDeferred list -> unit IDeferred
  val any : 'a IDeferred list -> 'a IDeferred
  val anyi : 'a IDeferred list -> ('a * int) IDeferred
  val anyUnit : 'a IDeferred list -> unit IDeferred
  val dontWaitFor : unit IDeferred -> unit

  // Conversion
  val ofAsync : 'a Async -> unit Async * 'a IDeferred
  val ofTask : 'a Task -> 'a IDeferred
  val ofTaskUnit : Task -> unit IDeferred

  module Infix =
    // Standard monad operators
    val (>>=) : 'a IDeferred -> ('a -> 'b IDeferred) -> 'b IDeferred
    val (>>|) : 'a IDeferred -> ('a -> 'b) -> 'b IDeferred

    val (>>>) : 'a IDeferred -> ('a -> unit) -> unit
