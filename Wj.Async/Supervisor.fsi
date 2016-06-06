namespace Wj.Async

open System

module Supervisor =
  // ISupervisor functions
  val parent : ISupervisor -> ISupervisor option
  val name : ISupervisor -> string
  val detach : ISupervisor -> unit
  val raise : ISupervisor -> ex : exn -> unit
  val uponException : ISupervisor -> handler : (exn -> unit) -> unit
  val uponException' : ISupervisor -> supervisor : ISupervisor * handler : (exn -> unit) -> unit
  val run : ISupervisor -> f : (unit -> 'a) -> Result.T<'a, exn>

  val root : ISupervisor
  val current : unit -> ISupervisor
  val create : unit -> ISupervisor
  val createNamed : name : string -> ISupervisor

  val supervise
    : f : (unit -> 'a IDeferred)
    -> observer : (exn -> unit)
    -> 'a IDeferred

  module AfterDetermined =
    type T = Raise | Log | Ignore

  val tryWith
    : f : (unit -> 'a IDeferred)
    -> handler : (exn -> 'a IDeferred)
    -> afterDetermined : AfterDetermined.T
    -> 'a IDeferred

  val tryFinally
    : f : (unit -> 'a IDeferred)
    -> finalizer : (unit -> unit IDeferred)
    -> 'a IDeferred
