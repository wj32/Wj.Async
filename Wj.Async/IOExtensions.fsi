namespace Wj.Async

open System
open System.IO
open System.Security.AccessControl
open System.Text

[<AutoOpen>]
module IOExtensions =
  type Stream with
    member FlushDeferred : unit -> unit IDeferred
    member ReadDeferred : buffer : byte array * ?offset : int * ?count : int -> int IDeferred
    member ReadDeferred : count : int -> byte array IDeferred
    member WriteDeferred : buffer : byte array * ?offset : int * ?count : int -> unit IDeferred

  type TextReader with
    member ReadDeferred : buffer : char array * ?index : int * ?count : int -> int IDeferred
    member ReadBlockDeferred : buffer : char array * ?index : int * ?count : int -> int IDeferred
    member ReadLineDeferred : unit -> string IDeferred
    member ReadToEndDeferred : unit -> string IDeferred

  type TextWriter with
    member FlushDeferred : unit -> unit IDeferred
    member WriteDeferred : value : char -> unit IDeferred
    member WriteDeferred : value : string -> unit IDeferred
    member WriteDeferred : buffer : char array * ?index : int * ?count : int -> unit IDeferred
    member WriteLineDeferred : value : char -> unit IDeferred
    member WriteLineDeferred : value : string -> unit IDeferred
    member WriteLineDeferred : buffer : char array * ?index : int * ?count : int -> unit IDeferred

  type File with
    static member AppendAllLinesDeferred : path : string * contents : string seq * ?encoding : Encoding -> unit IDeferred
    static member AppendAllTextDeferred : path : string * contents : string * ?encoding : Encoding -> unit IDeferred
    static member CopyDeferred : sourceFileName : string * destFileName : string * ?overwrite : bool -> unit IDeferred
    static member CreateDeferred : path : string * ?bufferSize : int * ?options : FileOptions * ?fileSecurity : FileSecurity -> FileStream IDeferred
    static member CreateTextDeferred : path : string -> StreamWriter IDeferred
    static member DecryptDeferred : path : string -> unit IDeferred
    static member DeleteDeferred : path : string -> unit IDeferred
    static member EncryptDeferred : path : string -> unit IDeferred
    static member ExistsDeferred : path : string -> bool IDeferred
    static member GetAccessControlDeferred : path : string * ?includeSections : AccessControlSections -> FileSecurity IDeferred
    static member GetAttributesDeferred : path : string -> FileAttributes IDeferred
    static member GetCreationTimeDeferred : path : string -> DateTime IDeferred
    static member GetCreationTimeUtcDeferred : path : string -> DateTime IDeferred
    static member GetLastAccessTimeDeferred : path : string -> DateTime IDeferred
    static member GetLastAccessTimeUtcDeferred : path : string -> DateTime IDeferred
    static member GetLastWriteTimeDeferred : path : string -> DateTime IDeferred
    static member GetLastWriteTimeUtcDeferred : path : string -> DateTime IDeferred
    static member MoveDeferred : sourceFileName : string * destFileName : string -> unit IDeferred
    static member OpenDeferred : path : string * mode : FileMode * ?access : FileAccess * ?share : FileShare * ?bufferSize : int -> FileStream IDeferred
    static member OpenReadDeferred : path : string -> FileStream IDeferred
    static member OpenTextDeferred : path : string * ?encoding : Encoding -> StreamReader IDeferred
    static member OpenWriteDeferred : path : string -> FileStream IDeferred
    static member ReadAllBytesDeferred : path : string -> byte array IDeferred
    static member ReadAllLinesDeferred : path : string * ?encoding : Encoding -> string array IDeferred
    static member ReadAllTextDeferred : path : string * ?encoding : Encoding -> string IDeferred
    static member ReadLinesDeferred : path : string * ?encoding : Encoding -> string Pipe.IReader
    static member ReplaceDeferred : sourceFileName : string * destFileName : string * destinationBackupFileName : string * ?ignoreMetadataErrors : bool -> unit IDeferred
    static member SetAccessControlDeferred : path : string * fileSecurity : FileSecurity -> unit IDeferred
    static member SetAttributesDeferred : path : string * fileAttributes : FileAttributes -> unit IDeferred
    static member SetCreationTimeDeferred : path : string * creationTime : DateTime -> unit IDeferred
    static member SetCreationTimeUtcDeferred : path : string * creationTimeUtc : DateTime -> unit IDeferred
    static member SetLastAccessTimeDeferred : path : string * lastAccessTime : DateTime -> unit IDeferred
    static member SetLastAccessTimeUtcDeferred : path : string * lastAccessTimeUtc : DateTime -> unit IDeferred
    static member SetLastWriteTimeDeferred : path : string * lastWriteTime : DateTime -> unit IDeferred
    static member SetLastWriteTimeUtcDeferred : path : string * lastWriteTimeUtc : DateTime -> unit IDeferred
    static member WriteAllBytesDeferred : path : string * bytes : byte array -> unit IDeferred
    static member WriteAllLinesDeferred : path : string * contents : string seq * ?encoding : Encoding -> unit IDeferred
    static member WriteAllTextDeferred : path : string * contents : string * ?encoding : Encoding -> unit IDeferred
