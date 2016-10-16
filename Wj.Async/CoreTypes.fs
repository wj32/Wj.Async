namespace Wj.Async

open System
open System.Collections.Generic

type [<Interface>] IDispatcher =
  inherit IDisposable

  abstract member Enqueue : supervisedCallback : unit SupervisedCallback -> unit
  abstract member Run : f : (unit -> 'a IDeferred) -> 'a
  abstract member RootSupervisor : ISupervisor
  abstract member RegisterScheduler : scheduler : IScheduler -> ISchedulerRegistration
and 'a SupervisedCallback = ISupervisor * ('a -> unit)
and [<Interface>] ISupervisor =
  abstract member Dispatcher : IDispatcher
  abstract member Parent : ISupervisor option
  abstract member Name : string
  abstract member Detach : unit -> unit
  abstract member SendException : ex : exn -> unit
  abstract member UponException : handler : (exn -> unit) -> unit
  abstract member UponException : supervisedHandler : exn SupervisedCallback -> unit
  abstract member TryRun : f : (unit -> 'a) -> Result.T<'a, exn>
and SchedulerCallback = ISupervisor * (unit -> unit IDeferred)
and [<Interface>] IScheduler =
  inherit IDisposable

  abstract member Dispatchers : IDispatcher ICollection
  abstract member AcceptingEnqueue : bool
  abstract member Enqueue : schedulerCallback : SchedulerCallback -> unit
  abstract member Take : ISchedulerTake option
and [<Interface>] ISchedulerTake =
  abstract member QueueDepth : int
  abstract member RegisterEnqueuedCallback : callback : (unit -> unit) -> IRegistration
  abstract member Dequeue : unit -> unit SupervisedCallback
and [<Interface>] ISchedulerRegistration =
  abstract member Update : unit -> unit
  abstract member Remove : unit -> unit
and [<Interface>] 'a IDeferred =
  abstract member Upon : callback : ('a -> unit) -> unit
  abstract member Upon : supervisedCallback : 'a SupervisedCallback -> unit
  abstract member Register : callback : ('a -> unit) -> IRegistration
  abstract member Register : supervisedCallback : 'a SupervisedCallback -> IRegistration
  abstract member MoveFrom : from : 'a SupervisedCallback RegistrationList.T -> unit
  abstract member IsDetermined : bool
  abstract member Get : unit -> 'a
  abstract member TryGet : unit -> 'a option

exception AfterDeterminedException of supervisorName : string * innerException : exn
exception SupervisorChildException of supervisorNames : string list * innerException : exn
exception SupervisorRootException of innerException : exn
