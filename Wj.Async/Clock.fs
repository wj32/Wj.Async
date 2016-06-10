namespace Wj.Async

open System
open System.Threading

module Clock =
  let [<Literal>] SpanCannotBeNegative = "The time span value cannot be negative."
  let [<Literal>] TimeCannotBePast = "The time span value cannot be negative."

  let afterTimer create =
    let v = Deferred.createVar ()
    let mutable timer : Timer = null
    let callback _ =
      Deferred.set v ()
      timer.Dispose()
    timer <- create callback
    v :> _ IDeferred

  let after (span : TimeSpan) =
    if span.Ticks < 0L then invalidArg "span" SpanCannotBeNegative
    afterTimer (fun callback -> new Timer(callback, (), span, Timeout.InfiniteTimeSpan))

  let afterMs ms =
    if ms < 0 then invalidArg "ms" SpanCannotBeNegative
    afterTimer (fun callback -> new Timer(callback, (), ms, Timeout.Infinite))

  let at time = after (time - DateTime.Now)
