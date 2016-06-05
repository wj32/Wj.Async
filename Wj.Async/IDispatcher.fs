namespace Wj.Async

[<Interface>]
type IDispatcher =
  abstract member Enqueue : f:(unit -> unit) -> unit
  abstract member Run : f:(unit -> 'a IDeferred) -> 'a
