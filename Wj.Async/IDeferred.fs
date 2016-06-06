namespace Wj.Async

[<Interface>]
type 'a IDeferred =
  abstract member Upon : callback : ('a -> unit) -> unit
  abstract member Upon : supervisedCallback : 'a SupervisedCallback -> unit
  abstract member Register : callback : ('a -> unit) -> IRegistration
  abstract member Register : supervisedCallback : 'a SupervisedCallback -> IRegistration
  abstract member MoveFrom : from : 'a SupervisedCallback RegistrationList.T -> unit
  abstract member IsDetermined : bool
  abstract member Get : unit -> 'a
  abstract member TryGet : unit -> 'a option
