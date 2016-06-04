namespace Wj.Async

open System.Threading;

module ThreadDispatcher =
  [<ReferenceEqualityAttribute>]
  type ThreadBlock =
    { mutable dispatchers : IDispatcher list; }

  let currentThreadBlock =
    let threadLocal = new ThreadLocal<ThreadBlock>(fun () ->
      { dispatchers = []; }
    )
    (fun () -> threadLocal.Value)

  let current () = (currentThreadBlock ()).dispatchers |> List.head

  let tryCurrent () = (currentThreadBlock ()).dispatchers |> List.tryHead

  let push dispatcher =
    let threadBlock = currentThreadBlock ()
    threadBlock.dispatchers <- dispatcher :: threadBlock.dispatchers

  let pop (dispatcher : IDispatcher) =
    let threadBlock = currentThreadBlock ()
    match threadBlock.dispatchers with
    | top :: rest when obj.ReferenceEquals(top, dispatcher) ->
      threadBlock.dispatchers <- rest
    | _ ->
      failwith "Thread dispatcher stack mismatch"
