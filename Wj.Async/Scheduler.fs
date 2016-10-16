namespace Wj.Async

open System.Collections.Generic

module Scheduler =
  // IScheduler functions
  let inline dispatchers (t : IScheduler) = t.Dispatchers
  let inline acceptingEnqueue (t : IScheduler) = t.AcceptingEnqueue
  let inline enqueue (t : IScheduler) schedulerCallback = t.Enqueue(schedulerCallback)
  let inline take (t : IScheduler) = t.Take
  let inline queueDepth (t : ISchedulerTake) = t.QueueDepth
  let inline registerEnqueuedCallback (t : ISchedulerTake) callback = t.RegisterEnqueuedCallback(callback)
  let inline dequeue (t : ISchedulerTake) = t.Dequeue()

  type 'a SynchronizedCollection =
    { lock : obj;
      collection : 'a ICollection; }

    interface System.Collections.IEnumerable with
      member t.GetEnumerator() = (t.collection :> System.Collections.IEnumerable).GetEnumerator()

    interface 'a IEnumerable with
      member t.GetEnumerator() = t.collection.GetEnumerator()

    interface 'a ICollection with
      member t.Count = t.collection.Count

      member t.IsReadOnly = t.collection.IsReadOnly

      member t.Add(x) = lock t.lock (fun () -> t.collection.Add(x))

      member t.Clear() = lock t.lock (fun () -> t.collection.Clear())

      member t.Contains(x) = lock t.lock (fun () -> t.collection.Contains(x))

      member t.CopyTo(a, i) = lock t.lock (fun () -> t.collection.CopyTo(a, i))

      member t.Remove(x) = lock t.lock (fun () -> t.collection.Remove(x))
