namespace Wj.Async

open System

type DeferredBuilder =
  new : unit -> DeferredBuilder
  member inline Bind : 'a IDeferred * ('a -> 'b IDeferred) -> 'b IDeferred
  member inline Combine : 'a IDeferred * ('a -> 'b IDeferred) -> 'b IDeferred
  member inline Delay : 'f -> 'f
  member inline Return : 'a -> 'a IDeferred
  member inline ReturnFrom : 'd -> 'd
  member inline Run : (unit -> 'd) -> 'd
  member inline Zero : unit -> unit IDeferred
  member inline TryFinally : body : (unit -> 'a IDeferred) * finalizer : (unit -> unit IDeferred) -> 'a IDeferred
  member inline TryWith : body : (unit -> 'a IDeferred) * handler : (exn -> 'a IDeferred) -> 'a IDeferred
  member inline Using : disposable : 'disposable * body : ('disposable -> 'a IDeferred) -> 'a IDeferred when 'disposable :> IDisposable
  member While : guard : (unit -> bool) * body : (unit -> unit IDeferred) -> unit IDeferred
  member inline For : xs : 'a seq * body : ('a -> unit IDeferred) -> unit IDeferred
  member inline For : xs : 'a DeferredSeq.T * body : ('a -> unit IDeferred) -> unit IDeferred
