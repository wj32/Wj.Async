namespace Wj.Async

[<Interface>]
type IDispatcher =
  abstract member Enqueue : supervisor : ISupervisor * f : (unit -> unit) -> unit
  abstract member Run : f : (unit -> 'a IDeferred) -> 'a
