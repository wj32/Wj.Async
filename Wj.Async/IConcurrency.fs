namespace Wj.Async

[<Interface>]
type IConcurrency =
  abstract member Enqueue : f : (unit -> 'a IDeferred) -> 'a IDeferred
