namespace Wj.Async

module Operators =
  let after = Clock.after
  let afterMs = Clock.afterMs
  let deferred = new DeferredBuilder()
  let dontWaitFor = Deferred.dontWaitFor
  let never = Deferred.never
  let ``return`` = Deferred.``return``
  let upon = Deferred.upon
