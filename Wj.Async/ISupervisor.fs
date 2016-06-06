namespace Wj.Async

open System

[<Interface>]
type ISupervisor =
  abstract member Parent : ISupervisor option
  abstract member Name : string
  abstract member Detach : unit -> unit
  abstract member Raise : ex : exn -> unit
  abstract member UponException : handler : (exn -> unit) -> unit
  abstract member UponException : supervisor : ISupervisor * handler : (exn -> unit) -> unit
  abstract member Run : f : (unit -> 'a) -> Result.T<'a, exn>
