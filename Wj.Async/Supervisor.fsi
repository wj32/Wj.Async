﻿namespace Wj.Async

open System

module Supervisor =
  // ISupervisor functions
  val parent : ISupervisor -> ISupervisor option
  val name : ISupervisor -> string
  val detach : ISupervisor -> unit
  val raise : ISupervisor -> ex : Exception -> unit
  val uponException : ISupervisor -> handler : (Exception -> unit) -> unit
  val uponException' : ISupervisor -> supervisor : ISupervisor * handler : (Exception -> unit) -> unit
  val run : ISupervisor -> f : (unit -> unit) -> unit

  val root : ISupervisor
  val current : unit -> ISupervisor
  val create : unit -> ISupervisor
  val createNamed : name : string -> ISupervisor

  val supervise
    : f : (unit -> 'a IDeferred)
    -> observer : (Exception -> unit)
    -> 'a IDeferred

  module AfterDetermined =
    type T = Raise | Log | Ignore

  val tryWith
    : f : (unit -> 'a IDeferred)
    -> handler : (Exception -> 'a IDeferred)
    -> afterDetermined : AfterDetermined.T
    -> 'a IDeferred

  val tryFinally
    : f : (unit -> 'a IDeferred)
    -> finalizer : (unit -> unit IDeferred)
    -> 'a IDeferred