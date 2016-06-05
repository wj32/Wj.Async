namespace Wj.Async

[<Interface>]
type 'a IDeferred =
  abstract member Upon : callback : ('a -> unit) -> unit
  abstract member Upon : supervisor : ISupervisor * callback : ('a -> unit) -> unit
  abstract member Get : unit -> 'a
  abstract member TryGet : unit -> 'a option
  abstract member IsDetermined : bool
