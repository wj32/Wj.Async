namespace Wj.Async

[<Interface>]
type IDispatcher =
  abstract member Enqueue : supervisedCallback : unit SupervisedCallback -> unit
  abstract member Run : f : (unit -> 'a IDeferred) -> 'a
  abstract member RootSupervisor : ISupervisor
