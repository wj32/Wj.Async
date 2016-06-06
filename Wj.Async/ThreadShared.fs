namespace Wj.Async

open System.Threading;

module internal ThreadShared =
  [<ReferenceEquality>]
  type ThreadBlock =
    { mutable supervisors : ISupervisor list;
      mutable dispatchers : IDispatcher list; }

  let currentThreadBlock =
    let threadLocal = new ThreadLocal<ThreadBlock>(fun () ->
      { supervisors = [];
        dispatchers = []; }
    )
    (fun () -> threadLocal.Value)

  let currentSupervisor () = (currentThreadBlock ()).supervisors |> List.head

  let tryCurrentSupervisor () = (currentThreadBlock ()).supervisors |> List.tryHead

  let pushSupervisor supervisor =
    let threadBlock = currentThreadBlock ()
    threadBlock.supervisors <- supervisor :: threadBlock.supervisors

  let popSupervisor (supervisor : ISupervisor) =
    let threadBlock = currentThreadBlock ()
    match threadBlock.supervisors with
    | top :: rest when obj.ReferenceEquals(top, supervisor) ->
      threadBlock.supervisors <- rest
    | _ ->
      failwith "Thread supervisor stack mismatch"

  let currentDispatcher () = (currentThreadBlock ()).dispatchers |> List.head

  let pushDispatcher dispatcher =
    let threadBlock = currentThreadBlock ()
    threadBlock.dispatchers <- dispatcher :: threadBlock.dispatchers

  let popDispatcher (dispatcher : IDispatcher) =
    let threadBlock = currentThreadBlock ()
    match threadBlock.dispatchers with
    | top :: rest when obj.ReferenceEquals(top, dispatcher) ->
      threadBlock.dispatchers <- rest
    | _ ->
      failwith "Thread dispatcher stack mismatch"
