namespace Wj.Async

open System
open System.Collections.Specialized
open System.IO
open System.Net

[<AutoOpen>]
module NetExtensions =
  type DPCEA = DownloadProgressChangedEventArgs
  type UPCEA = UploadProgressChangedEventArgs

  type WebClient with
    member DownloadDataDeferred : address : Uri -> byte array IDeferred
    member DownloadDataDeferredWithProgress : address : Uri -> byte array IDeferred * DPCEA DeferredSeq.T
    member DownloadFileDeferred : address : Uri * fileName : string -> unit IDeferred
    member DownloadFileDeferredWithProgress : address : Uri * fileName : string -> unit IDeferred * DPCEA DeferredSeq.T
    member DownloadStringDeferred : address : Uri -> string IDeferred
    member DownloadStringDeferredWithProgress : address : Uri -> string IDeferred * DPCEA DeferredSeq.T
    member OpenReadDeferred : address : Uri -> Stream IDeferred
    member OpenWriteDeferred : address : Uri * requestMethod : string -> Stream IDeferred
    member OpenWriteDeferred : address : Uri -> Stream IDeferred
    member UploadDataDeferred : address : Uri * requestMethod : string * data : byte array -> byte array IDeferred
    member UploadDataDeferred : address : Uri * data : byte array -> byte array IDeferred
    member UploadDataDeferredWithProgress : address : Uri * requestMethod : string * data : byte array -> byte array IDeferred * UPCEA DeferredSeq.T
    member UploadDataDeferredWithProgress : address : Uri * data : byte array -> byte array IDeferred * UPCEA DeferredSeq.T
    member UploadFileDeferred : address : Uri * requestMethod : string * fileName : string -> byte array IDeferred
    member UploadFileDeferred : address : Uri * fileName : string -> byte array IDeferred
    member UploadFileDeferredWithProgress : address : Uri * requestMethod : string * fileName : string -> byte array IDeferred * UPCEA DeferredSeq.T
    member UploadFileDeferredWithProgress : address : Uri * fileName : string -> byte array IDeferred * UPCEA DeferredSeq.T
    member UploadStringDeferred : address : Uri * requestMethod : string * data : string -> string IDeferred
    member UploadStringDeferred : address : Uri * data : string -> string IDeferred
    member UploadStringDeferredWithProgress : address : Uri * requestMethod : string * data : string -> string IDeferred * UPCEA DeferredSeq.T
    member UploadStringDeferredWithProgress : address : Uri * data : string -> string IDeferred * UPCEA DeferredSeq.T
    member UploadValuesDeferred : address : Uri * requestMethod : string * data : NameValueCollection -> byte array IDeferred
    member UploadValuesDeferred : address : Uri * data : NameValueCollection -> byte array IDeferred
    member UploadValuesDeferredWithProgress : address : Uri * requestMethod : string * data : NameValueCollection -> byte array IDeferred * UPCEA DeferredSeq.T
    member UploadValuesDeferredWithProgress : address : Uri * data : NameValueCollection -> byte array IDeferred * UPCEA DeferredSeq.T

  type WebRequest with
    member GetRequestStreamDeferred : unit -> Stream IDeferred
    member GetResponseDeferred : unit -> WebResponse IDeferred
