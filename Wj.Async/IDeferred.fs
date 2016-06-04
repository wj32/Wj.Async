namespace Wj.Async

[<Interface>]
type IDeferred<'a> =
  abstract member Upon : ('a -> unit) -> unit
  abstract member Get : unit -> 'a
  abstract member TryGet : unit -> 'a option
  abstract member IsDetermined : bool
