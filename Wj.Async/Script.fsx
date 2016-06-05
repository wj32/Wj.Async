// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "IDeferred.fs"
#load "IVar.fs"
#load "INode.fs"
#load "IDispatcher.fs"
#load "ThreadDispatcher.fs"
#load "Deferred.fs"
#load "Dispatcher.fs"
#load "Clock.fs"

open Wj.Async
open System.Threading.Tasks

let dispatcher = Dispatcher.create ()

Dispatcher.run dispatcher (fun () ->
  let root =
    Deferred.bind (Clock.afterMs 2000) (fun () ->
      printfn "Done!"
      let rec loop counter =
        Deferred.bind (Clock.afterMs 2000) (fun () ->
          printfn "After another 2 seconds: iteration %d" counter
          loop (counter + 1)
        )
      loop 1
    )
  root
)

printfn "Dispatcher stopped."
stdin.ReadLine()
