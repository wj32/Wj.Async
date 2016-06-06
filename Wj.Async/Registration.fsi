namespace Wj.Async

open System.Collections.Generic

module Registration =
  val empty : IRegistration
  val fromLinkedListNode : _ LinkedListNode -> IRegistration
