namespace Wj.Async

[<Interface>]
type INode<'a> =
  inherit IDeferred<'a>

  abstract member Link : parent : 'a IDeferred -> unit
