namespace Wj.Async

open System

[<Interface>]
type ISupervisor =
  abstract member Parent : ISupervisor option
  abstract member Name : string
  abstract member Detach : unit -> unit
  abstract member SendException : ex : exn -> unit
  abstract member UponException : handler : (exn -> unit) -> unit
  abstract member UponException : supervisedHandler : exn SupervisedCallback -> unit
  abstract member Run : f : (unit -> 'a) -> Result.T<'a, exn>
and 'a SupervisedCallback = ISupervisor * ('a -> unit)
