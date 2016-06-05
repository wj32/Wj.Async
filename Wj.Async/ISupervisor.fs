namespace Wj.Async

open System

[<Interface>]
type ISupervisor =
  abstract member Parent : ISupervisor option
  abstract member Name : string
  abstract member Detach : unit -> unit
  abstract member Raise : ex : Exception -> unit
  abstract member UponException : handler : (Exception -> unit) -> unit
  abstract member UponException : supervisor : ISupervisor * handler : (Exception -> unit) -> unit
  abstract member Run : f : (unit -> unit) -> unit
