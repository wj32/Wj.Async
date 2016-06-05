// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "IDeferred.fs"
#load "IVar.fs"
#load "INode.fs"
#load "IDispatcher.fs"
#load "ThreadDispatcher.fs"
#load "Deferred.fs"
#load "Dispatcher.fs"
#load "DeferredBuilder.fs"
#load "Clock.fs"
#load "Operators.fs"

open Wj.Async
open Wj.Async.Deferred.Infix
open Wj.Async.Operators
open System.Threading.Tasks

let dispatcher = Dispatcher.create ()

Dispatcher.run dispatcher (fun () -> deferred {
  let mutable counter = 1
  printfn "Starting!"
  let! _ = afterMs 1000
  printfn "..."
  dontWaitFor (deferred {
    while true do
      do! afterMs 900
      printfn "Every 900 milliseconds!!!"
  })
  for _ in {1 .. 10} do
    do! afterMs 2000
    printfn "After another 2 seconds: iteration %d" counter
    if counter % 2 = 0 then
      printfn "EVEN"
    counter <- counter + 1
  return ()
})

printfn "Dispatcher stopped."

if false then
  Dispatcher.run dispatcher (fun () ->
    printfn "Starting!"
    Deferred.unit
    >>= fun () ->
    afterMs 2000
    >>= fun() ->
    printfn "Waited 2 seconds."
    printfn "Returning 1234."
    ``return`` 1234
  )
  |> printfn "%d"