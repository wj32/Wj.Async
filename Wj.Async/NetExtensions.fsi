namespace Wj.Async

open System
open System.Collections.Specialized
open System.IO
open System.Net
open System.Net.WebSockets
open System.Security.Cryptography.X509Certificates
open System.Threading

[<AutoOpen>]
module NetExtensions =
  type DPCEA = DownloadProgressChangedEventArgs
  type UPCEA = UploadProgressChangedEventArgs

  type Dns with
    static member GetHostAddresses : hostNameOrAddress : string -> IPAddress array IDeferred
    static member GetHostEntry : hostNameOrAddress : string -> IPHostEntry IDeferred
    static member GetHostEntry : address : IPAddress -> IPHostEntry IDeferred

  type HttpListener with
    member GetContextDeferred : unit -> HttpListenerContext IDeferred

  type HttpListenerContext with
    member AcceptWebSocketDeferred : subProtocol : string * ?keepAliveInterval : TimeSpan * ?receiveBufferSize : int * ?internalBuffer : byte ArraySegment -> HttpListenerWebSocketContext IDeferred

  type HttpListenerRequest with
    member GetClientCertificateDeferred : unit -> X509Certificate2 IDeferred

  type WebClient with
    member DownloadDataDeferred : address : Uri -> byte array IDeferred
    member DownloadDataDeferredWithProgress : address : Uri -> byte array IDeferred * DPCEA Pipe.IReader
    member DownloadFileDeferred : address : Uri * fileName : string -> unit IDeferred
    member DownloadFileDeferredWithProgress : address : Uri * fileName : string -> unit IDeferred * DPCEA Pipe.IReader
    member DownloadStringDeferred : address : Uri -> string IDeferred
    member DownloadStringDeferredWithProgress : address : Uri -> string IDeferred * DPCEA Pipe.IReader
    member OpenReadDeferred : address : Uri -> Stream IDeferred
    member OpenWriteDeferred : address : Uri * ?requestMethod : string -> Stream IDeferred
    member UploadDataDeferred : address : Uri * data : byte array * ?requestMethod : string -> byte array IDeferred
    member UploadDataDeferredWithProgress : address : Uri * data : byte array * ?requestMethod : string -> byte array IDeferred * UPCEA Pipe.IReader
    member UploadFileDeferred : address : Uri * fileName : string * ?requestMethod : string -> byte array IDeferred
    member UploadFileDeferredWithProgress : address : Uri * fileName : string * ?requestMethod : string -> byte array IDeferred * UPCEA Pipe.IReader
    member UploadStringDeferred : address : Uri * data : string * ?requestMethod : string -> string IDeferred
    member UploadStringDeferredWithProgress : address : Uri * data : string * ?requestMethod : string -> string IDeferred * UPCEA Pipe.IReader
    member UploadValuesDeferred : address : Uri * data : NameValueCollection * ?requestMethod : string -> byte array IDeferred
    member UploadValuesDeferredWithProgress : address : Uri * data : NameValueCollection * ?requestMethod : string -> byte array IDeferred * UPCEA Pipe.IReader

  type WebRequest with
    member GetRequestStreamDeferred : unit -> Stream IDeferred
    member GetResponseDeferred : unit -> WebResponse IDeferred

  type WebSocket with
    member CloseDeferred : closeStatus : WebSocketCloseStatus * statusDescription : string * ?cancellationToken : CancellationToken -> unit IDeferred
    member CloseOutputDeferred : closeStatus : WebSocketCloseStatus * statusDescription : string * ?cancellationToken : CancellationToken -> unit IDeferred
    member ReceiveDeferred : buffer : byte ArraySegment * ?cancellationToken : CancellationToken -> WebSocketReceiveResult IDeferred
    member SendDeferred : buffer : byte ArraySegment * messageType : WebSocketMessageType * endOfMessage : bool * ?cancellationToken : CancellationToken -> unit IDeferred
