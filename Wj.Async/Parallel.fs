namespace Wj.Async

open System
open Wj.Async.Internal

module Parallel =
  [<Interface>]
  type 'a IDequeueable =
    abstract member Lock : obj
    abstract member IsEmpty : bool
    abstract member Dequeue : unit -> 'a

  module DequeuingDispatcher =
    [<ReferenceEquality>]
    type T =
      { mutable rootSupervisor : ISupervisor;
        sharedQueue : unit SupervisedCallback IDequeueable;
        privateQueue : unit SupervisedCallback Queue.T; }

  module Pool =
    type T =
      | Dummy

    module Config =
      type T =
        { minThreadCount : int;
          maxThreadCount : int;
          threadTimeout : TimeSpan option; }

      let ``default`` =
        { minThreadCount = 0;
          maxThreadCount = Environment.ProcessorCount;
          threadTimeout = Some (TimeSpan.FromSeconds(10.)); }

    let create (_ : Config.T) =
      Dummy

    let ``default`` = create Config.``default``

  let parallelDefault = Concurrency.concurrent

  let parallelAtMost (_ : int) = (failwith "Not implemented") : IConcurrency
