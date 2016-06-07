﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Result.fs"
#load "IRegistration.fs"
#load "Registration.fs"
#load "RegistrationList.fs"
#load "CoreTypes.fs"
#load "IVar.fs"
#load "INode.fs"
#load "ThreadShared.fs"
#load "Deferred.fs"
#load "Supervisor.fs"
#load "Dispatcher.fs"
#load "DeferredBuilder.fs"
#load "Clock.fs"
#load "Operators.fs"

open Wj.Async
open Wj.Async.Deferred.Infix
open Wj.Async.Operators
open System.Threading.Tasks

let dispatcher = Dispatcher.create ()

let testBind () =
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

let testExceptions () =
  try
    Dispatcher.run dispatcher (fun () -> deferred {
      let mutable counter = 1
      printfn "Starting!"
      let! _ = afterMs 1000
      printfn "..."
      try
        for _ in {1 .. 10} do
          do! afterMs 2000
          printfn "After another 2 seconds: iteration %d" counter

          try
            try
              dontWaitFor (Supervisor.supervise
                (fun () -> deferred {
                  printfn "Running code in dontWaitFor"
                  do! afterMs 1000
                  do!
                    Supervisor.supervise
                      (fun () -> deferred {
                        do! afterMs 500
                        printfn "My supervisor is %s." (ThreadShared.currentSupervisor()).Name
                        printfn "My parent supervisor is %s." (ThreadShared.currentSupervisor()).Parent.Value.Name
                        raise (invalidOp "Exception in inner supervisor!")
                      })
                      (fun ex -> printfn "Inner observer saw exception: %s" (string ex))
                })
                (fun ex -> printfn "Observer saw exception: %s" (string ex))
              )
              do! Deferred.unit
              raise (invalidOp "Exception!")
              do! Deferred.unit
            finally
              printfn "Executing finally!"
          with ex ->
            printfn "Caught exception: %s" (string ex)

          try
            try
              do! Deferred.unit
              raise (invalidOp "Exception 2!")
              do! Deferred.unit
            finally
              printfn "Executing finally!"
              raise (invalidOp "Finalizer exception!")
          with ex ->
            do! Deferred.unit
            printfn "Caught exception in try or finally: %s" (string ex)

          try
            if counter % 2 = 0 then
              printfn "EVEN"
              do! afterMs 100
              raise (invalidOp "Test exception!")
              do! Deferred.unit
          with ex ->
            do! afterMs 100
            printfn "Caught exception: %s" (string ex)
            try
              do! Deferred.unit
              raise (invalidOp "Exception in exception handler that will be caught!")
              do! Deferred.unit
            with ex ->
              do! Deferred.unit
              printfn "Caught exception in exception handler that will be caught: %s" (string ex)
            do! Deferred.unit
            raise (invalidOp "Exception in exception handler!")

          counter <- counter + 1
      with ex ->
        printfn "Caught exception in exception handler: %s" (string ex)
        do! afterMs 2000
      return ()
    })
  with ex ->
    printfn "Uncaught exception: %s" (string ex)

  printfn "Dispatcher stopped."

let wait ms = deferred {
  do! afterMs ms
  printfn "I waited %d ms." ms
}

let testAll () =
  Dispatcher.run dispatcher (fun () -> deferred {
    do! Deferred.allUnit [wait 1000; wait 100; wait 200; wait 5]
    printfn "All done."
  })

let testAny () =
  Dispatcher.run dispatcher (fun () -> deferred {
    let list = [never (); wait 1000; wait 100; wait 200]
    let! ((), i) = Deferred.anyi list
    printfn "Index %i finished first." i
    do! Deferred.allUnit (List.tail list)
  })

let testChoice () =
  Dispatcher.run dispatcher (fun () -> deferred {
    let! result =
      choose
        [ choice (Deferred.map (afterMs 100) (fun () -> 0)) ((+) 1);
          choice (Deferred.map (afterMs 200) (fun () -> 2)) ((+) 3);
          choice (Deferred.map (afterMs 5) (fun () -> 4)) ((+) 5); ]
    printfn "9 = %d" result
    do!
      choose
        [ choice (afterMs 100) (fun () -> printfn "%s" "This should display");
          choice (afterMs 200) (fun () -> printfn "%s" "This should not display"); ]
    let v1 = Deferred.createVar ()
    let v2 = Deferred.createVar ()
    dontWaitFor (deferred {
      do! afterMs 100
      Deferred.set v2 ()
      Deferred.set v1 ()
    })
    do!
      choose
        [ choice v1 (fun () -> printfn "%s" "This should display");
          choice (afterMs 200) (fun () -> printfn "%s" "This should not display");
          choice v2 (fun () -> printfn "%s" "This should not display"); ]
  })
