﻿namespace Wj.Async

open System

module DeferredBuilder =
  type 'a T = 'a IDeferred
  type 'a M = 'a T

  type B =
    new : unit -> B
    member inline Bind : 'a M * ('a -> 'b M) -> 'b M
    member inline Combine : unit M * (unit -> 'b M) -> 'b M
    member inline Delay : 'f -> 'f
    member inline Return : 'a -> 'a M
    member inline ReturnFrom : 'm -> 'm
    member inline Run : (unit -> 'm) -> 'm
    member inline Zero : unit -> unit M
    member inline TryFinally : body : (unit -> 'a M) * finalizer : (unit -> unit IDeferred) -> 'a M
    member inline TryFinally : body : (unit -> 'a M) * finalizer : (unit -> unit) -> 'a M
    member inline TryWith : body : (unit -> 'a M) * handler : (exn -> 'a M) -> 'a M
    member inline Using : disposable : 'disposable * body : ('disposable -> 'a M) -> 'a M when 'disposable :> IDisposable
    member inline While : guard : (unit -> bool) * body : (unit -> unit M) -> unit M
    member inline While : guard : (unit -> bool IDeferred) * body : (unit -> unit M) -> unit M
    member inline For : xs : 'a seq * body : ('a -> unit M) -> unit M
    member inline For : xs : 'a DeferredSeq.T * body : ('a -> unit M) -> unit M
