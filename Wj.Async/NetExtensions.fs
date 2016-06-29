namespace Wj.Async

open System
open System.ComponentModel
open System.IO
open System.Net
open System.Net.WebSockets
open System.Security.Cryptography.X509Certificates
open System.Threading

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
              v <-- createResult args
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
    let (writer, reader) = Pipe.create ()
    let mutable handler = null
    let closeWriterAndRemoveHandler () =
      match handler with
      | null -> ()
      | _ ->
        Pipe.close writer
        progressEvent.RemoveHandler(handler)
        handler <- null
    let d =
      webClientAsyncInner (fun token ->
        let f _ (args : #ProgressChangedEventArgs) =
          if obj.ReferenceEquals(args.UserState, token) then
            if Pipe.isClosed writer then
              closeWriterAndRemoveHandler ()
            else
              Pipe.writeImmediately writer args
        handler <- createProgressHandler f
        progressEvent.AddHandler(handler)
      ) closeWriterAndRemoveHandler event createHandler start createResult
    (d, reader)

  type Dns with
    static member GetHostAddresses(hostNameOrAddress) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostAddresses(hostNameOrAddress, callback, null))
        Dns.EndGetHostAddresses

    static member GetHostEntry(hostNameOrAddress : string) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostEntry(hostNameOrAddress, callback, null))
        Dns.EndGetHostEntry

    static member GetHostEntry(address : IPAddress) =
      Deferred.ofBeginEnd
        (fun callback -> Dns.BeginGetHostEntry(address, callback, null))
        Dns.EndGetHostEntry

  type HttpListener with
    member t.GetContextDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetContext(callback, null))
        t.EndGetContext

  type HttpListenerContext with
    member t.AcceptWebSocketDeferred
      (subProtocol, ?keepAliveInterval, ?receiveBufferSize, ?internalBuffer)
      =
      let keepAliveInterval = defaultArg keepAliveInterval WebSocket.DefaultKeepAliveInterval
      let receiveBufferSize = defaultArg receiveBufferSize 16384
      match internalBuffer with
      | Some internalBuffer ->
        Deferred.ofTask (
          t.AcceptWebSocketAsync(subProtocol, receiveBufferSize, keepAliveInterval, internalBuffer)
        )
      | None ->
        Deferred.ofTask (t.AcceptWebSocketAsync(subProtocol, receiveBufferSize, keepAliveInterval))

  type HttpListenerRequest with
    member t.GetClientCertificateDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetClientCertificate(callback, null))
        t.EndGetClientCertificate

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
        (fun callback -> t.BeginGetRequestStream(callback, null))
        t.EndGetRequestStream

    member t.GetResponseDeferred() =
      Deferred.ofBeginEnd
        (fun callback -> t.BeginGetResponse(callback, null))
        t.EndGetResponse

  type WebSocket with
    member t.CloseDeferred(closeStatus, statusDescription, ?cancellationToken) =
      let cancellationToken = defaultArg cancellationToken CancellationToken.None
      Deferred.ofTaskUnit (t.CloseAsync(closeStatus, statusDescription, cancellationToken))

    member t.CloseOutputDeferred(closeStatus, statusDescription, ?cancellationToken) =
      let cancellationToken = defaultArg cancellationToken CancellationToken.None
      Deferred.ofTaskUnit (t.CloseOutputAsync(closeStatus, statusDescription, cancellationToken))

    member t.ReceiveDeferred(buffer, ?cancellationToken) =
      let cancellationToken = defaultArg cancellationToken CancellationToken.None
      Deferred.ofTask (t.ReceiveAsync(buffer, cancellationToken))

    member t.SendDeferred(buffer, messageType, endOfMessage, ?cancellationToken) =
      let cancellationToken = defaultArg cancellationToken CancellationToken.None
      Deferred.ofTaskUnit (t.SendAsync(buffer, messageType, endOfMessage, cancellationToken))
