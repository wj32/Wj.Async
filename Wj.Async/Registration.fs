namespace Wj.Async

open System.Collections.Generic

module Registration =
  // IRegistration functions
  let inline remove (t : IRegistration) = t.Remove()

  let empty =
    { new IRegistration with
        member this.Remove() = ()
    }

  let fromLinkedListNode (node : _ LinkedListNode) =
    let mutable node = node
    { new IRegistration with
      member this.Remove() =
        match node with
        | null -> ()
        | _ ->
          node.List.Remove(node)
          node <- null
    }
