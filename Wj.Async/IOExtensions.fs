namespace Wj.Async

open System
open System.IO
open System.Security.AccessControl
open System.Text

[<AutoOpen>]
module IOExtensions =
  open Deferred.Infix
  open Operators

  let [<Literal>] FileTooLong2GB =
    "The file is too long. This operation is currently limited to supporting files less than 2 gigabytes in size."

  let utf8NoBomEncoding = new UTF8Encoding(false, true) :> Encoding

  let defaultFileSystemRights =
    FileSystemRights.ReadData ||| FileSystemRights.WriteData ||| FileSystemRights.AppendData |||
    FileSystemRights.ReadExtendedAttributes ||| FileSystemRights.WriteExtendedAttributes |||
    FileSystemRights.ReadAttributes ||| FileSystemRights.WriteAttributes |||
    FileSystemRights.ReadPermissions

  let defaultAccessControlSections =
    AccessControlSections.Access ||| AccessControlSections.Owner ||| AccessControlSections.Group

  type Stream with
    member t.FlushDeferred(?cancellation) =
      Deferred.ofTaskUnit (t.FlushAsync(Cancellation.Option.toToken cancellation))

    member t.ReadDeferred(buffer, ?offset, ?count, ?cancellation : Cancellation.T) =
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      match cancellation with
      | Some cancellation ->
        Deferred.ofTask (t.ReadAsync(buffer, offset, count, Cancellation.toToken cancellation))
      | None ->
        Deferred.ofBeginEnd
          (fun callback -> t.BeginRead(buffer, offset, count, callback, null))
          t.EndRead

    member t.ReadDeferred(count, ?cancellation) =
      let buffer = Array.zeroCreate count
      t.ReadDeferred(buffer, 0, count, ?cancellation = cancellation)
      >>| fun bytesRead ->
      if bytesRead = count then
        buffer
      else
        Array.sub buffer 0 bytesRead

    member t.WriteDeferred(buffer, ?offset, ?count, ?cancellation : Cancellation.T) =
      let offset = defaultArg offset 0
      let count = defaultArg count (Array.length buffer)
      match cancellation with
      | Some cancellation ->
        Deferred.ofTaskUnit (t.WriteAsync(buffer, offset, count, Cancellation.toToken cancellation))
      | None ->
        Deferred.ofBeginEnd
          (fun callback -> t.BeginWrite(buffer, offset, count, callback, null))
          t.EndWrite

    member t.CopyToDeferred(destination : Stream, ?bufferSize, ?cancellation) =
      let bufferSize = defaultArg bufferSize (20 * 4096)
      let buffer = Array.zeroCreate bufferSize
      let rec loop () = cancellableDeferred cancellation {
        let! readBytes = t.ReadDeferred(buffer, ?cancellation = cancellation)
        if readBytes <> 0 then
          do! destination.WriteDeferred(buffer, count = readBytes, ?cancellation = cancellation)
          do! loop ()
      }
      loop ()

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

  type File with
    static member inline WrapSimple x f = Deferred.start (fun () -> f x)

    static member inline WrapSimple2 x y f = Deferred.start (fun () -> f(x, y))

    static member AppendAllLinesDeferred(path, contents : string seq, ?encoding, ?cancellation) =
      let encoding = defaultArg encoding utf8NoBomEncoding
      cancellableDeferred cancellation {
        use! writer = Deferred.start (fun () -> new StreamWriter(path, true, encoding))
        for line in contents do
          do! writer.WriteLineDeferred(line)
      }

    static member AppendAllTextDeferred(path, contents : string, ?encoding, ?cancellation) =
      let encoding = defaultArg encoding utf8NoBomEncoding
      cancellableDeferred cancellation {
        use! writer = Deferred.start (fun () -> new StreamWriter(path, true, encoding))
        do! writer.WriteDeferred(contents)
      }

    static member CopyDeferred(sourceFileName, destFileName, ?overwrite) =
      let overwrite = defaultArg overwrite false
      Deferred.start (fun () -> File.Copy(sourceFileName, destFileName, overwrite))

    static member CreateDeferred(path, bufferSize, options, fileSecurity) =
      let bufferSize = defaultArg bufferSize 4096
      let options = defaultArg options FileOptions.None
      match fileSecurity with
      | Some fileSecurity ->
        Deferred.start (fun () ->
          new FileStream(path, FileMode.Create, defaultFileSystemRights, FileShare.None, bufferSize,
            options, fileSecurity)
        )
      | None ->
        Deferred.start (fun () ->
          new FileStream(path, FileMode.Create, FileAccess.ReadWrite, FileShare.None, bufferSize,
            options)
        )

    static member CreateTextDeferred(path) = File.CreateText |> File.WrapSimple path

    static member DecryptDeferred(path) = File.Decrypt |> File.WrapSimple path

    static member DeleteDeferred(path) = File.Delete |> File.WrapSimple path

    static member EncryptDeferred(path) = File.Encrypt |> File.WrapSimple path

    static member ExistsDeferred(path) = File.Exists |> File.WrapSimple path

    static member GetAccessControlDeferred(path, ?includeSections) =
      let includeSections = defaultArg includeSections defaultAccessControlSections
      Deferred.start (fun () -> File.GetAccessControl(path, includeSections))

    static member GetAttributesDeferred(path) = File.GetAttributes |> File.WrapSimple path

    static member GetCreationTimeDeferred(path) = File.GetCreationTime |> File.WrapSimple path

    static member GetCreationTimeUtcDeferred(path) = File.GetCreationTimeUtc |> File.WrapSimple path

    static member GetLastAccessTimeDeferred(path) = File.GetLastAccessTime |> File.WrapSimple path

    static member GetLastAccessTimeUtcDeferred(path) = File.GetLastAccessTimeUtc |> File.WrapSimple path

    static member GetLastWriteTimeDeferred(path) = File.GetLastWriteTime |> File.WrapSimple path

    static member GetLastWriteTimeUtcDeferred(path) = File.GetLastWriteTimeUtc |> File.WrapSimple path

    static member MoveDeferred(sourceFileName, destFileName) =
      Deferred.start (fun () -> File.Move(sourceFileName, destFileName))

    static member OpenDeferred(path, mode, ?access, ?share, ?bufferSize) =
      let access =
        defaultArg access (if mode = FileMode.Append then FileAccess.Write else FileAccess.ReadWrite)
      let share = defaultArg share FileShare.None
      let bufferSize = defaultArg bufferSize 4096
      Deferred.start (fun () -> new FileStream(path, mode, access, share, bufferSize))

    static member OpenReadDeferred(path) = File.OpenRead |> File.WrapSimple path

    static member OpenTextDeferred(path : string, ?encoding) =
      let encoding = defaultArg encoding Encoding.UTF8
      Deferred.start (fun () -> new StreamReader(path, encoding))

    static member OpenWriteDeferred(path) = File.OpenWrite |> File.WrapSimple path

    static member ReadAllBytesDeferred(path, ?cancellation) =
      cancellableDeferred cancellation {
        use! stream = File.OpenDeferred(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096)
        if stream.Length > int64 Int32.MaxValue then
          raise (new IOException(FileTooLong2GB))
        let mutable remaining = int stream.Length
        let mutable index = 0
        let buffer = Array.zeroCreate remaining
        while remaining > 0 do
          let! bytesRead = stream.ReadDeferred(buffer, index, remaining, ?cancellation = cancellation)
          if bytesRead = 0 then
            raise (new EndOfStreamException())
          index <- index + bytesRead
          remaining <- remaining - bytesRead
        return buffer
      }

    static member ReadAllLinesDeferred(path, ?encoding) =
      let encoding = defaultArg encoding Encoding.UTF8
      (File.ReadAllLines : string * Encoding -> _) |> File.WrapSimple2 path encoding

    static member ReadAllTextDeferred(path, ?encoding) =
      let encoding = defaultArg encoding Encoding.UTF8
      (File.ReadAllText : string * Encoding -> _) |> File.WrapSimple2 path encoding

    static member ReadLinesDeferred(path, ?encoding) =
      Pipe.createReader (fun writer -> deferred {
        Pipe.setCapacity writer 1
        use! reader = File.OpenTextDeferred(path, ?encoding = encoding)
        let rec loop () =
          reader.ReadLineDeferred()
          >>= function
          | null -> Deferred.unit
          | line ->
            if Pipe.isClosed writer then
              Deferred.unit
            else
              Pipe.write writer line >>= loop
        do! loop ()
      })

    static member ReplaceDeferred
      (sourceFileName, destFileName, destinationBackupFileName, ?ignoreMetadataErrors)
      =
      let ignoreMetadataErrors = defaultArg ignoreMetadataErrors false
      Deferred.start (fun () ->
        File.Replace(sourceFileName, destFileName, destinationBackupFileName, ignoreMetadataErrors)
      )

    static member SetAccessControlDeferred(path, fileSecurity) =
      File.SetAccessControl |> File.WrapSimple2 path fileSecurity

    static member SetAttributesDeferred(path, fileAttributes) =
      File.SetAttributes |> File.WrapSimple2 path fileAttributes

    static member SetCreationTimeDeferred(path, creationTime) =
      File.SetCreationTime |> File.WrapSimple2 path creationTime

    static member SetCreationTimeUtcDeferred(path, creationTimeUtc) =
      File.SetCreationTimeUtc |> File.WrapSimple2 path creationTimeUtc

    static member SetLastAccessTimeDeferred(path, lastAccessTime) =
      File.SetLastAccessTime |> File.WrapSimple2 path lastAccessTime

    static member SetLastAccessTimeUtcDeferred(path, lastAccessTimeUtc) =
      File.SetLastAccessTimeUtc |> File.WrapSimple2 path lastAccessTimeUtc

    static member SetLastWriteTimeDeferred(path, lastWriteTime) =
      File.SetLastWriteTime |> File.WrapSimple2 path lastWriteTime

    static member SetLastWriteTimeUtcDeferred(path, lastWriteTimeUtc) =
      File.SetLastWriteTimeUtc |> File.WrapSimple2 path lastWriteTimeUtc

    static member WriteAllBytesDeferred(path, bytes, ?cancellation) =
      deferred {
        use! stream = File.OpenDeferred(path, FileMode.Create, FileAccess.Write, FileShare.Read, 4096)
        do! stream.WriteDeferred(bytes, ?cancellation = cancellation)
      }

    static member WriteAllLinesDeferred(path, contents : string seq, ?encoding, ?cancellation) =
      let encoding = defaultArg encoding utf8NoBomEncoding
      cancellableDeferred cancellation {
        use! writer = Deferred.start (fun () -> new StreamWriter(path, false, encoding))
        for line in contents do
          do! writer.WriteLineDeferred(line)
      }

    static member WriteAllTextDeferred(path, contents : string, ?encoding) =
      let encoding = defaultArg encoding utf8NoBomEncoding
      deferred {
        use! writer = Deferred.start (fun () -> new StreamWriter(path, false, encoding, 1024))
        do! writer.WriteDeferred(contents)
      }
