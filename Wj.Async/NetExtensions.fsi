﻿namespace Wj.Async

open System
open System.Collections.Specialized
open System.IO
open System.Net
open System.Net.WebSockets
open System.Security.Cryptography.X509Certificates

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
    member DownloadDataDeferred : address : Uri * ?cancellation : Cancellation.T -> byte array IDeferred
    member DownloadDataDeferredWithProgress : address : Uri * ?cancellation : Cancellation.T -> byte array IDeferred * DPCEA Pipe.IReader
    member DownloadFileDeferred : address : Uri * fileName : string * ?cancellation : Cancellation.T -> unit IDeferred
    member DownloadFileDeferredWithProgress : address : Uri * fileName : string * ?cancellation : Cancellation.T -> unit IDeferred * DPCEA Pipe.IReader
    member DownloadStringDeferred : address : Uri * ?cancellation : Cancellation.T -> string IDeferred
    member DownloadStringDeferredWithProgress : address : Uri * ?cancellation : Cancellation.T -> string IDeferred * DPCEA Pipe.IReader
    member OpenReadDeferred : address : Uri * ?cancellation : Cancellation.T -> Stream IDeferred
    member OpenWriteDeferred : address : Uri * ?requestMethod : string * ?cancellation : Cancellation.T -> Stream IDeferred
    member UploadDataDeferred : address : Uri * data : byte array * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred
    member UploadDataDeferredWithProgress : address : Uri * data : byte array * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred * UPCEA Pipe.IReader
    member UploadFileDeferred : address : Uri * fileName : string * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred
    member UploadFileDeferredWithProgress : address : Uri * fileName : string * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred * UPCEA Pipe.IReader
    member UploadStringDeferred : address : Uri * data : string * ?requestMethod : string * ?cancellation : Cancellation.T -> string IDeferred
    member UploadStringDeferredWithProgress : address : Uri * data : string * ?requestMethod : string * ?cancellation : Cancellation.T -> string IDeferred * UPCEA Pipe.IReader
    member UploadValuesDeferred : address : Uri * data : NameValueCollection * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred
    member UploadValuesDeferredWithProgress : address : Uri * data : NameValueCollection * ?requestMethod : string * ?cancellation : Cancellation.T -> byte array IDeferred * UPCEA Pipe.IReader

  type WebRequest with
    member GetRequestStreamDeferred : unit -> Stream IDeferred
    member GetResponseDeferred : unit -> WebResponse IDeferred

  type WebSocket with
    member CloseDeferred : closeStatus : WebSocketCloseStatus * statusDescription : string * ?cancellation : Cancellation.T -> unit IDeferred
    member CloseOutputDeferred : closeStatus : WebSocketCloseStatus * statusDescription : string * ?cancellation : Cancellation.T -> unit IDeferred
    member ReceiveDeferred : buffer : byte ArraySegment * ?cancellation : Cancellation.T -> WebSocketReceiveResult IDeferred
    member SendDeferred : buffer : byte ArraySegment * messageType : WebSocketMessageType * endOfMessage : bool * ?cancellation : Cancellation.T -> unit IDeferred
