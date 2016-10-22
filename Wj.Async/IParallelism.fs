namespace Wj.Async

[<Interface>]
type IParallelism =
  abstract member Enqueue : f : (unit -> 'a IDeferred) -> 'a IDeferred
