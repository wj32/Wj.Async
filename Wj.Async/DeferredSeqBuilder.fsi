namespace Wj.Async

open System

module DeferredSeqBuilder =
  type 'a T = 'a DeferredSeq.T
  type 'a M = 'a DeferredSeq.Writer.T -> unit IDeferred

  type B =
    new : unit -> B
    member inline Bind : 'b IDeferred * ('b -> 'a M) -> 'a M
    member inline Combine : 'a M * 'a M -> 'a M
    member inline Delay : (unit -> 'a M) -> 'a M
    member inline Run : 'a M -> 'a T
    member inline Yield : 'a -> 'a M
    member inline YieldFrom : 'a T -> 'a M
    member inline Zero : unit -> 'a M
    member inline TryFinally : body : 'a M * finalizer : (unit -> unit IDeferred) -> 'a M
    member inline TryFinally : body : 'a M * finalizer : (unit -> unit) -> 'a M
    member inline TryWith : body : 'a M * handler : (exn -> 'a M) -> 'a M
    member inline Using : disposable : 'disposable * body : ('disposable -> 'a M) -> 'a M when 'disposable :> IDisposable
    member inline While : guard : (unit -> bool) * body : 'a M -> 'a M
    member inline While : guard : (unit -> bool IDeferred) * body : 'a M -> 'a M
    member inline For : xs : 'b seq * body : ('b -> 'a M) -> 'a M
    member inline For : xs : 'b T * body : ('b -> 'a M) -> 'a M
