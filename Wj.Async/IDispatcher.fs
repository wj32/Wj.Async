namespace Wj.Async

[<Interface>]
type IDispatcher =
  abstract member Enqueue : (unit -> unit) -> unit
  abstract member Run : (unit -> 'a IDeferred) -> 'a
