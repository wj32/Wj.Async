namespace Wj.Async

open System.IO

[<AutoOpen>]
module IOExtensions =
  open Deferred.Infix

  type Stream with
    member t.FlushDeferred() =
      let supervisor = Supervisor.current ()
      Deferred.create (fun v ->
        System.Threading.ThreadPool.QueueUserWorkItem(fun _ ->
          try
            t.Flush() --> v
          with ex ->
            supervisor.SendException(ex)
        ) |> ignore
      )

    member t.ReadDeferred(buffer, ?offset, ?count) =
      let supervisor = Supervisor.current ()
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      Deferred.create (fun v ->
        t.BeginRead(buffer, offset, count, (fun result ->
          try
            t.EndRead(result) --> v
          with ex ->
            supervisor.SendException(ex)
        ), null) |> ignore
      )

    member t.ReadDeferred(count) =
      let buffer = Array.zeroCreate count
      t.ReadDeferred(buffer, 0, count)
      >>| fun bytesRead ->
      if bytesRead = count then
        buffer
      else
        Array.sub buffer 0 bytesRead

    member t.WriteDeferred(buffer, ?offset, ?count) =
      let supervisor = Supervisor.current ()
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      Deferred.create (fun v ->
        t.BeginWrite(buffer, offset, count, (fun result ->
          try
            t.EndWrite(result) --> v
          with ex ->
            supervisor.SendException(ex)
        ), null) |> ignore
      )
