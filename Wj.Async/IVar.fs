namespace Wj.Async

[<Interface>]
type 'a IVar =
  inherit ('a IDeferred)

  abstract member Set : value : 'a -> unit
  abstract member TrySet : value : 'a -> bool
