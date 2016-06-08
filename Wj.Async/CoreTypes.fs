namespace Wj.Async

open System

type [<Interface>] IDispatcher =
  abstract member Enqueue : supervisedCallback : unit SupervisedCallback -> unit
  abstract member Run : f : (unit -> 'a IDeferred) -> 'a
  abstract member RootSupervisor : ISupervisor
and [<Interface>] ISupervisor =
  abstract member Dispatcher : IDispatcher
  abstract member Parent : ISupervisor option
  abstract member Name : string
  abstract member Detach : unit -> unit
  abstract member SendException : ex : exn -> unit
  abstract member UponException : handler : (exn -> unit) -> unit
  abstract member UponException : supervisedHandler : exn SupervisedCallback -> unit
  abstract member Run : f : (unit -> 'a) -> Result.T<'a, exn>
and 'a SupervisedCallback = ISupervisor * ('a -> unit)
and [<Interface>] 'a IDeferred =
  abstract member Upon : callback : ('a -> unit) -> unit
  abstract member Upon : supervisedCallback : 'a SupervisedCallback -> unit
  abstract member Register : callback : ('a -> unit) -> IRegistration
  abstract member Register : supervisedCallback : 'a SupervisedCallback -> IRegistration
  abstract member MoveFrom : from : 'a SupervisedCallback RegistrationList.T -> unit
  abstract member IsDetermined : bool
  abstract member Get : unit -> 'a
  abstract member TryGet : unit -> 'a option

exception SupervisorChildException of supervisorNames : string list * innerException : exn
