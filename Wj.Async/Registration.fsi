namespace Wj.Async

open System.Collections.Generic

module Registration =
  // IRegistration functions
  val remove : IRegistration -> unit

  val empty : IRegistration
  val fromLinkedListNode : _ LinkedListNode -> IRegistration
