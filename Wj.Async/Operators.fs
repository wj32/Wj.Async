namespace Wj.Async

module Operators =
  let after = Clock.after
  let afterMs = Clock.afterMs
  let choice = Deferred.choice
  let choose = Deferred.choose
  let deferred = new DeferredBuilder.B()
  let dontWaitFor = Deferred.dontWaitFor
  let never = Deferred.never
  let now = Deferred.unit
  let ``return`` = Deferred.``return``
  let upon = Deferred.upon
