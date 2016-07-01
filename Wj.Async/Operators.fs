namespace Wj.Async

[<AutoOpen>]
module Operators =
  let after = Clock.after
  let afterMs = Clock.afterMs
  let inline cancellableDeferred cancellable = new CancellableDeferredBuilder.B(cancellable)
  let choice = Deferred.choice
  let choose = Deferred.choose
  let deferred = new DeferredBuilder.B()
  let deferredSeq = new DeferredSeqBuilder.B()
  let dontWaitFor = Deferred.dontWaitFor
  let never = Deferred.never
  let now = Deferred.unit
  let ``return`` = Deferred.``return``
  let upon = Deferred.upon
