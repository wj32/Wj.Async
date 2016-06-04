namespace Wj.Async

[<Interface>]
type IVar<'a> =
  inherit IDeferred<'a>

  abstract member Set : 'a -> unit
  abstract member TrySet : 'a -> bool
