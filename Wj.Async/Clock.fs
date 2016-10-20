namespace Wj.Async

open System
open System.Threading

module Clock =
  let [<Literal>] SpanCannotBeNegative = "The time span value cannot be negative."
  let [<Literal>] TimeCannotBePast = "The time span value cannot be negative."

  let inline afterTimer create =
    let dispatcher = Dispatcher.current ()
    let v = Deferred.createVar ()
    let mutable timer : Timer = null
    let callback _ =
      Dispatcher.enqueueRoot dispatcher (fun () -> Deferred.set v ())
      timer.Dispose()
    timer <- create callback
    v :> _ IDeferred

  let after (span : TimeSpan) =
    if span.Ticks > 0L then
      afterTimer (fun callback -> new Timer(callback, (), span, Timeout.InfiniteTimeSpan))
    else if span.Ticks = 0L then
      Deferred.unit
    else
      invalidArg "span" SpanCannotBeNegative

  let afterMs ms =
    if ms > 0 then
      afterTimer (fun callback -> new Timer(callback, (), ms, Timeout.Infinite))
    else if ms = 0 then
      Deferred.unit
    else
      invalidArg "ms" SpanCannotBeNegative

  let at time = after (time - DateTime.Now)
