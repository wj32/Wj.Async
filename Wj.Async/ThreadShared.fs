namespace Wj.Async

open System.Collections.Generic;
open System.Threading;

module internal ThreadShared =
  [<ReferenceEquality>]
  type ThreadBlock =
    { supervisors : ISupervisor Stack;
      dispatchers : IDispatcher Stack; }

  let currentThreadBlock =
    let threadLocal = new ThreadLocal<ThreadBlock>(fun () ->
      { supervisors = new Stack<ISupervisor>();
        dispatchers = new Stack<IDispatcher>(); }
    )
    (fun () -> threadLocal.Value)

  let currentSupervisor () = (currentThreadBlock ()).supervisors.Peek()

  let tryCurrentSupervisor () =
    let supervisors = (currentThreadBlock ()).supervisors
    if supervisors.Count <> 0 then
      Some (supervisors.Peek())
    else
      None

  let pushSupervisor supervisor = (currentThreadBlock ()).supervisors.Push(supervisor)

  let popSupervisor (supervisor : ISupervisor) =
    let supervisors = (currentThreadBlock ()).supervisors
    let top = supervisors.Pop()
    if not (obj.ReferenceEquals(top, supervisor)) then
      failwith "Thread supervisor stack mismatch"

  let currentDispatcher () = (currentThreadBlock ()).dispatchers.Peek()

  let pushDispatcher dispatcher = (currentThreadBlock ()).dispatchers.Push(dispatcher)

  let popDispatcher (dispatcher : IDispatcher) =
    let dispatchers = (currentThreadBlock ()).dispatchers
    let top = dispatchers.Pop()
    if not (obj.ReferenceEquals(top, dispatcher)) then
      failwith "Thread dispatcher stack mismatch"
