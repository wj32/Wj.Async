﻿namespace Wj.Async

open System
open System.Collections.Specialized
open System.IO
open System.Net

[<AutoOpen>]
module NetExtensions =
  type DPCEA = DownloadProgressChangedEventArgs
  type UPCEA = UploadProgressChangedEventArgs

  type Dns with
    static member GetHostAddresses : hostNameOrAddress : string -> IPAddress array IDeferred
    static member GetHostEntry : hostNameOrAddress : string -> IPHostEntry IDeferred
    static member GetHostEntry : address : IPAddress -> IPHostEntry IDeferred

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