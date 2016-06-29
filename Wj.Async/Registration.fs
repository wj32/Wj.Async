namespace Wj.Async

open System.Collections.Generic

module Registration =
  // IRegistration functions
  let inline remove (t : IRegistration) = t.Remove()

  let empty =
    { new IRegistration with
        member this.Remove() = ()
    }
