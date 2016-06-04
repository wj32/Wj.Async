namespace Wj.Async

module ThreadDispatcher =
  val current : unit -> IDispatcher
  val tryCurrent : unit -> IDispatcher option
  val push : IDispatcher -> unit
  val pop : IDispatcher -> unit
