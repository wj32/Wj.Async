namespace Wj.Async

module internal ThreadShared =
  val currentSupervisor : unit -> ISupervisor
  val tryCurrentSupervisor : unit -> ISupervisor option
  val pushSupervisor : supervisor : ISupervisor -> unit
  val popSupervisor : supervisor : ISupervisor -> unit

  val currentDispatcher : unit -> IDispatcher
  val pushDispatcher : dispatcher : IDispatcher -> unit
  val popDispatcher : dispatcher : IDispatcher -> unit
