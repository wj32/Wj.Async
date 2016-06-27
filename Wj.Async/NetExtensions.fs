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
          completed ()
          event.RemoveHandler(handler)
          if args.Cancelled then
            supervisor.SendException(new OperationCanceledException())
          else if args.Error <> null then
            supervisor.SendException(args.Error)
          else
            createResult args --> v
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

    member t.OpenWriteDeferred(address, requestMethod) =
      webClientAsync
        t.OpenWriteCompleted
        (fun f -> new OpenWriteCompletedEventHandler(f))
        (fun token -> t.OpenWriteAsync(address, requestMethod, token))
        (fun args -> args.Result)

    member t.OpenWriteDeferred(address : Uri) = t.OpenWriteDeferred(address, null)

    member inline t.UploadAsyncWithProgress event createHandler start createResult =
      webClientAsyncWithProgress t.UploadProgressChanged
        (fun f -> new UploadProgressChangedEventHandler(f)) event createHandler start createResult

    member inline t.UploadDataArgs address requestMethod data f =
      f
        t.UploadDataCompleted
        (fun f -> new UploadDataCompletedEventHandler(f))
        (fun token -> t.UploadDataAsync(address, requestMethod, data))
        (fun (args : UploadDataCompletedEventArgs) -> args.Result)

    member t.UploadDataDeferred(address, requestMethod, data) =
      webClientAsync |> t.UploadDataArgs address requestMethod data

    member t.UploadDataDeferred(address, data) = t.UploadDataDeferred(address, null, data)

    member t.UploadDataDeferredWithProgress(address, requestMethod, data) =
      t.UploadAsyncWithProgress |> t.UploadDataArgs address requestMethod data

    member t.UploadDataDeferredWithProgress(address, data) =
      t.UploadDataDeferredWithProgress(address, null, data)

    member inline t.UploadFileArgs address requestMethod fileName f =
      f
        t.UploadFileCompleted
        (fun f -> new UploadFileCompletedEventHandler(f))
        (fun token -> t.UploadFileAsync(address, requestMethod, fileName))
        (fun (args : UploadFileCompletedEventArgs) -> args.Result)

    member t.UploadFileDeferred(address, requestMethod, fileName) =
      webClientAsync |> t.UploadFileArgs address requestMethod fileName

    member t.UploadFileDeferred(address, fileName) = t.UploadFileDeferred(address, null, fileName)

    member t.UploadFileDeferredWithProgress(address, requestMethod, fileName) =
      t.UploadAsyncWithProgress |> t.UploadFileArgs address requestMethod fileName

    member t.UploadFileDeferredWithProgress(address, fileName) =
      t.UploadFileDeferredWithProgress(address, null, fileName)

    member inline t.UploadStringArgs address requestMethod data f =
      f
        t.UploadStringCompleted
        (fun f -> new UploadStringCompletedEventHandler(f))
        (fun token -> t.UploadStringAsync(address, requestMethod, data))
        (fun (args : UploadStringCompletedEventArgs) -> args.Result)

    member t.UploadStringDeferred(address, requestMethod, data) =
      webClientAsync |> t.UploadStringArgs address requestMethod data

    member t.UploadStringDeferred(address, data) = t.UploadStringDeferred(address, null, data)

    member t.UploadStringDeferredWithProgress(address, requestMethod, data) =
      t.UploadAsyncWithProgress |> t.UploadStringArgs address requestMethod data

    member t.UploadStringDeferredWithProgress(address, data) =
      t.UploadStringDeferredWithProgress(address, null, data)

    member inline t.UploadValuesArgs address requestMethod data f =
      f
        t.UploadValuesCompleted
        (fun f -> new UploadValuesCompletedEventHandler(f))
        (fun token -> t.UploadValuesAsync(address, requestMethod, data))
        (fun (args : UploadValuesCompletedEventArgs) -> args.Result)

    member t.UploadValuesDeferred(address, requestMethod, data) =
      webClientAsync |> t.UploadValuesArgs address requestMethod data

    member t.UploadValuesDeferred(address, data) = t.UploadValuesDeferred(address, null, data)

    member t.UploadValuesDeferredWithProgress(address, requestMethod, data) =
      t.UploadAsyncWithProgress |> t.UploadValuesArgs address requestMethod data

    member t.UploadValuesDeferredWithProgress(address, data) =
      t.UploadValuesDeferredWithProgress(address, null, data)

  type WebRequest with
    member t.GetRequestStreamDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetRequestStream(callback, null) |> ignore)
        t.EndGetRequestStream

    member t.GetResponseDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetResponse(callback, null) |> ignore)
        t.EndGetResponse
