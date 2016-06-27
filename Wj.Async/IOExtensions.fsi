namespace Wj.Async

open System.IO

[<AutoOpen>]
module IOExtensions =
  type Stream with
    member FlushDeferred : unit -> unit IDeferred
    member ReadDeferred : buffer : byte array * ?offset : int * ?count : int -> int IDeferred
    member ReadDeferred : count : int -> byte array IDeferred
    member WriteDeferred : buffer : byte array * ?offset : int * ?count : int -> unit IDeferred

  type TextReader with
    member ReadDeferred : buffer : char array * ?index : int * ?count : int -> int IDeferred
    member ReadBlockDeferred : buffer : char array * ?index : int * ?count : int -> int IDeferred
    member ReadLineDeferred : unit -> string IDeferred
    member ReadToEndDeferred : unit -> string IDeferred

  type TextWriter with
    member FlushDeferred : unit -> unit IDeferred
    member WriteDeferred : value : char -> unit IDeferred
    member WriteDeferred : value : string -> unit IDeferred
    member WriteDeferred : buffer : char array * ?index : int * ?count : int -> unit IDeferred
    member WriteLineDeferred : value : char -> unit IDeferred
    member WriteLineDeferred : value : string -> unit IDeferred
    member WriteLineDeferred : buffer : char array * ?index : int * ?count : int -> unit IDeferred
