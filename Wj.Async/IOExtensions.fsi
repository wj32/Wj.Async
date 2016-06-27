namespace Wj.Async

open System.IO

[<AutoOpen>]
module IOExtensions =
  type Stream with
    member FlushDeferred : unit -> unit IDeferred
    member ReadDeferred : buffer : byte array * ?offset : int * ?count : int -> int IDeferred
    member ReadDeferred : count : int -> byte array IDeferred
    member WriteDeferred : buffer : byte array * ?offset : int * ?count : int -> unit IDeferred
