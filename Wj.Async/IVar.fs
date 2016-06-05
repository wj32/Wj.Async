namespace Wj.Async

[<Interface>]
type IVar<'a> =
  inherit IDeferred<'a>

  abstract member Set : value : 'a -> unit
  abstract member TrySet : value : 'a -> bool
