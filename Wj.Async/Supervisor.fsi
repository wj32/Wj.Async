namespace Wj.Async

open System

exception SupervisorChildException of supervisorNames : string list * innerException : exn

module Supervisor =
  // ISupervisor functions
  val dispatcher : ISupervisor -> IDispatcher
  val parent : ISupervisor -> ISupervisor option
  val name : ISupervisor -> string
  val detach : ISupervisor -> unit
  val sendException : ISupervisor -> ex : exn -> unit
  val uponException : ISupervisor -> handler : (exn -> unit) -> unit
  val uponException' : ISupervisor -> supervisedHandler : exn SupervisedCallback -> unit
  val run : ISupervisor -> f : (unit -> 'a) -> Result.T<'a, exn>

  val current : unit -> ISupervisor
  val create : unit -> ISupervisor
  val createNamed : name : string -> ISupervisor
  val createRoot : IDispatcher -> ISupervisor

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
