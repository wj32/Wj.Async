namespace Wj.Async

open System.IO

[<AutoOpen>]
module IOExtensions =
  open Deferred.Infix

  type Stream with
    member t.FlushDeferred() = Deferred.ofTaskUnit (t.FlushAsync())

    member t.ReadDeferred(buffer, ?offset, ?count) =
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofBeginEnd
        (fun callback -> t.BeginRead(buffer, offset, count, callback, null) |> ignore)
        t.EndRead

    member t.ReadDeferred(count) =
      let buffer = Array.zeroCreate count
      t.ReadDeferred(buffer, 0, count)
      >>| fun bytesRead ->
      if bytesRead = count then
        buffer
      else
        Array.sub buffer 0 bytesRead

    member t.WriteDeferred(buffer, ?offset, ?count) =
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofBeginEnd
        (fun callback -> t.BeginWrite(buffer, offset, count, callback, null) |> ignore)
        t.EndWrite

  type TextReader with
    member t.ReadDeferred(buffer, ?index, ?count) =
      let index = defaultArg index 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofTask (t.ReadAsync(buffer, index, count))

    member t.ReadBlockDeferred(buffer, ?index, ?count) =
      let index = defaultArg index 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofTask (t.ReadBlockAsync(buffer, index, count))

    member t.ReadLineDeferred() = Deferred.ofTask (t.ReadLineAsync())

    member t.ReadToEndDeferred() = Deferred.ofTask (t.ReadToEndAsync())

  type TextWriter with
    member t.FlushDeferred() = Deferred.ofTaskUnit (t.FlushAsync())

    member t.WriteDeferred(value : char) = Deferred.ofTaskUnit (t.WriteAsync(value))

    member t.WriteDeferred(value : string) = Deferred.ofTaskUnit (t.WriteAsync(value))

    member t.WriteDeferred(buffer, ?index, ?count) =
      let index = defaultArg index 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofTaskUnit (t.WriteAsync(buffer, index, count))

    member t.WriteLineDeferred(value : char) = Deferred.ofTaskUnit (t.WriteLineAsync(value))

    member t.WriteLineDeferred(value : string) = Deferred.ofTaskUnit (t.WriteLineAsync(value))

    member t.WriteLineDeferred(buffer, ?index, ?count) =
      let index = defaultArg index 0
      let count = defaultArg count (Array.length buffer)
      Deferred.ofTaskUnit (t.WriteLineAsync(buffer, index, count))
