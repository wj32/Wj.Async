namespace Wj.Async

open System
open System.ComponentModel
open System.IO
open System.Net

[<AutoOpen>]
module NetExtensions =
  open Deferred.Infix

  type DPCEA = DownloadProgressChangedEventArgs
  type UPCEA = UploadProgressChangedEventArgs

  let inline webClientAsyncInner
    beforeStart completed (event : IEvent<_, _>) createHandler start createResult
    =
    let supervisor = Supervisor.current ()
    Deferred.create (fun v ->
      let token = new obj()
      let mutable handler = null
      let f _ (args : #AsyncCompletedEventArgs) =
        if obj.ReferenceEquals(args.UserState, token) then
          supervisor.Dispatcher.Enqueue((supervisor, fun () ->
            completed ()
            event.RemoveHandler(handler)
            if args.Cancelled then
              supervisor.SendException(new OperationCanceledException())
            else if args.Error <> null then
              supervisor.SendException(args.Error)
            else
              createResult args --> v
          ))
      handler <- createHandler f
      event.AddHandler(handler)
      beforeStart token
      start token
    )

  let inline webClientAsync event createHandler start createResult =
    webClientAsyncInner (fun token -> ()) id event createHandler start createResult

  let inline webClientAsyncWithProgress
    (progressEvent : IEvent<_, _>) createProgressHandler event createHandler start createResult
    =
    let writer = DeferredSeq.Writer.create ()
    let progress = DeferredSeq.Writer.read writer
    let mutable handler = null
    let d =
      webClientAsyncInner (fun token ->
        let f _ (args : #ProgressChangedEventArgs) =
          if obj.ReferenceEquals(args.UserState, token) then
            DeferredSeq.Writer.write writer args
        handler <- createProgressHandler f
        progressEvent.AddHandler(handler)
      ) (fun () -> progressEvent.RemoveHandler(handler)) event createHandler start createResult
    (d, progress)

  type Dns with
    static member GetHostAddresses(hostNameOrAddress) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostAddresses(hostNameOrAddress, callback, null) |> ignore)
        Dns.EndGetHostAddresses

    static member GetHostEntry(hostNameOrAddress : string) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostEntry(hostNameOrAddress, callback, null) |> ignore)
        Dns.EndGetHostEntry

    static member GetHostEntry(address : IPAddress) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostEntry(address, callback, null) |> ignore)
        Dns.EndGetHostEntry

  type WebClient with
    member inline t.DownloadAsyncWithProgress event createHandler start createResult =
      webClientAsyncWithProgress t.DownloadProgressChanged
        (fun f -> new DownloadProgressChangedEventHandler(f)) event createHandler start createResult

    member inline t.DownloadDataArgs address action =
      action
        t.DownloadDataCompleted
        (fun f -> new DownloadDataCompletedEventHandler(f))
        (fun token -> t.DownloadDataAsync(address, token))
        (fun (args : DownloadDataCompletedEventArgs) -> args.Result)

    member t.DownloadDataDeferred(address) =
      webClientAsync |> t.DownloadDataArgs address

    member t.DownloadDataDeferredWithProgress(address) =
      t.DownloadAsyncWithProgress |> t.DownloadDataArgs address

    member inline t.DownloadFileArgs address fileName f =
      f
        t.DownloadFileCompleted
        (fun f -> new AsyncCompletedEventHandler(f))
        (fun token -> t.DownloadFileAsync(address, fileName, token))
        (fun (args : AsyncCompletedEventArgs) -> ())

    member t.DownloadFileDeferred(address, fileName) =
      webClientAsync |> t.DownloadFileArgs address fileName

    member t.DownloadFileDeferredWithProgress(address, fileName) =
      t.DownloadAsyncWithProgress |> t.DownloadFileArgs address fileName

    member inline t.DownloadStringArgs address f =
      f
        t.DownloadStringCompleted
        (fun f -> new DownloadStringCompletedEventHandler(f))
        (fun token -> t.DownloadStringAsync(address, token))
        (fun (args : DownloadStringCompletedEventArgs) -> args.Result)

    member t.DownloadStringDeferred(address) =
      webClientAsync |> t.DownloadStringArgs address

    member t.DownloadStringDeferredWithProgress(address) =
      t.DownloadAsyncWithProgress |> t.DownloadStringArgs address

    member t.OpenReadDeferred(address) =
      webClientAsync
        t.OpenReadCompleted
        (fun f -> new OpenReadCompletedEventHandler(f))
        (fun token -> t.OpenReadAsync(address, token))
        (fun args -> args.Result)

    member t.OpenWriteDeferred(address, ?requestMethod) =
      webClientAsync
        t.OpenWriteCompleted
        (fun f -> new OpenWriteCompletedEventHandler(f))
        (fun token -> t.OpenWriteAsync(address, defaultArg requestMethod null, token))
        (fun args -> args.Result)

    member inline t.UploadAsyncWithProgress event createHandler start createResult =
      webClientAsyncWithProgress t.UploadProgressChanged
        (fun f -> new UploadProgressChangedEventHandler(f)) event createHandler start createResult

    member inline t.UploadDataArgs address data requestMethod f =
      f
        t.UploadDataCompleted
        (fun f -> new UploadDataCompletedEventHandler(f))
        (fun token -> t.UploadDataAsync(address, defaultArg requestMethod null, data))
        (fun (args : UploadDataCompletedEventArgs) -> args.Result)

    member t.UploadDataDeferred(address, data, ?requestMethod) =
      webClientAsync |> t.UploadDataArgs address data requestMethod

    member t.UploadDataDeferredWithProgress(address, data, ?requestMethod) =
      t.UploadAsyncWithProgress |> t.UploadDataArgs address data requestMethod

    member inline t.UploadFileArgs address fileName requestMethod f =
      f
        t.UploadFileCompleted
        (fun f -> new UploadFileCompletedEventHandler(f))
        (fun token -> t.UploadFileAsync(address, defaultArg requestMethod null, fileName))
        (fun (args : UploadFileCompletedEventArgs) -> args.Result)

    member t.UploadFileDeferred(address, fileName, ?requestMethod) =
      webClientAsync |> t.UploadFileArgs address fileName requestMethod

    member t.UploadFileDeferredWithProgress(address, fileName, ?requestMethod) =
      t.UploadAsyncWithProgress |> t.UploadFileArgs address fileName requestMethod

    member inline t.UploadStringArgs address data requestMethod f =
      f
        t.UploadStringCompleted
        (fun f -> new UploadStringCompletedEventHandler(f))
        (fun token -> t.UploadStringAsync(address, defaultArg requestMethod null, data))
        (fun (args : UploadStringCompletedEventArgs) -> args.Result)

    member t.UploadStringDeferred(address, data, ?requestMethod) =
      webClientAsync |> t.UploadStringArgs address data requestMethod

    member t.UploadStringDeferredWithProgress(address, data, ?requestMethod) =
      t.UploadAsyncWithProgress |> t.UploadStringArgs address data requestMethod

    member inline t.UploadValuesArgs address data requestMethod f =
      f
        t.UploadValuesCompleted
        (fun f -> new UploadValuesCompletedEventHandler(f))
        (fun token -> t.UploadValuesAsync(address, defaultArg requestMethod null, data))
        (fun (args : UploadValuesCompletedEventArgs) -> args.Result)

    member t.UploadValuesDeferred(address, data, ?requestMethod) =
      webClientAsync |> t.UploadValuesArgs address data requestMethod

    member t.UploadValuesDeferredWithProgress(address, data, ?requestMethod) =
      t.UploadAsyncWithProgress |> t.UploadValuesArgs address data requestMethod

  type WebRequest with
    member t.GetRequestStreamDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetRequestStream(callback, null) |> ignore)
        t.EndGetRequestStream

    member t.GetResponseDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetResponse(callback, null) |> ignore)
        t.EndGetResponse
