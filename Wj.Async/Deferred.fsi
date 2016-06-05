namespace Wj.Async

open System.Threading.Tasks;

module Deferred =
  // IDeferred functions
  val upon : 'a IDeferred -> ('a -> unit) -> unit
  val get : 'a IDeferred -> 'a
  val tryGet : 'a IDeferred -> 'a option
  val isDetermined : 'a IDeferred -> bool
  // IVar functions
  val set : 'a IVar -> 'a -> unit
  val trySet : 'a IVar -> 'a -> bool
  // INode functions
  val link : 'a INode -> 'a IDeferred -> unit

  val create : 'a -> 'a IDeferred
  val createVar : unit -> 'a IVar
  val createNode : unit -> 'a INode
  val unit : unit IDeferred
  val never : unit -> 'a IDeferred

  // Monad
  val ``return`` : 'a -> 'a IDeferred // Same as create
  val bind : 'a IDeferred -> ('a -> 'b IDeferred) -> 'b IDeferred

  // Standard monad functions
  val map : 'a IDeferred -> ('a -> 'b) -> 'b IDeferred
  val join : 'a IDeferred IDeferred -> 'a IDeferred
  val forget : 'a IDeferred -> unit IDeferred
  val all : 'a IDeferred list -> 'a list IDeferred
  val allForget : unit IDeferred list -> unit IDeferred

  // Extra convenience functions
  val both : 'a IDeferred -> 'b IDeferred -> ('a * 'b) IDeferred
  val allUnit : unit IDeferred list -> unit IDeferred
  val any : 'a IDeferred list -> 'a IDeferred
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
