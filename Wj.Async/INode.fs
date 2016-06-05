namespace Wj.Async

[<Interface>]
type 'a INode =
  inherit ('a IDeferred)

  abstract member IsLinked : bool
  abstract member Link : parent : 'a IDeferred -> unit
  abstract member TryLink : parent : 'a IDeferred -> bool
