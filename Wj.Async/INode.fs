namespace Wj.Async

[<Interface>]
type 'a INode =
  inherit ('a IDeferred)

  abstract member Link : parent : 'a IDeferred -> unit
