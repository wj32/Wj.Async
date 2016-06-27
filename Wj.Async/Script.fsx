// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Result.fs"
#load "IRegistration.fs"
#load "Registration.fs"
#load "RegistrationList.fs"
#load "Queue.fs"
#load "CoreTypes.fs"
#load "Parallelism.fs"
#load "IVar.fs"
#load "ThreadShared.fs"
#load "ChildSupervisor.fs"
#load "Deferred.fs"
#load "DeferredSeq.fs"
#load "Supervisor.fs"
#load "Dispatcher.fs"
#load "Clock.fs"
#load "DeferredBuilder.fs"
#load "DeferredSeqBuilder.fs"
#load "Pipe.fs"
#load "IOExtensions.fs"
#load "NetExtensions.fs"
#load "Operators.fs"

open Wj.Async
open Wj.Async.Deferred.Infix
open System.Threading.Tasks

let dispatcher = Dispatcher.create ()

let testBind () =
  Dispatcher.run dispatcher (fun () ->
    printfn "Starting!"
    Deferred.unit
    >>= fun () ->
    afterMs 2000
    >>= fun () ->
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
                        try
                          invalidOp "Exception in inner supervisor!"
                        finally
                          printfn "Finally in inner supervisor!"
                          Deferred.unit
                      })
                      (fun ex -> printfn "Inner observer saw exception: %A" ex)
                })
                (fun ex -> printfn "Observer saw exception: %A" ex)
              )
              do! Deferred.unit
              invalidOp "Exception!"
              do! Deferred.unit
            finally
              printfn "Executing finally!"
              Deferred.unit
          with ex ->
            printfn "Caught exception: %A" ex

          try
            try
              do! Deferred.unit
              invalidOp "Exception 2!"
              do! Deferred.unit
            finally
              printfn "Executing finally!"
              invalidOp "Finalizer exception!"
              ()
          with ex ->
            do! Deferred.unit
            printfn "Caught exception in try or finally: %A" ex

          try
            if counter % 2 = 0 then
              printfn "EVEN"
              do! afterMs 100
              invalidOp "Test exception!"
              do! Deferred.unit
          with ex ->
            do! afterMs 100
            printfn "Caught exception: %A" ex
            try
              do! Deferred.unit
              invalidOp "Exception in exception handler that will be caught!"
              do! Deferred.unit
            with ex ->
              do! Deferred.unit
              printfn "Caught exception in exception handler that will be caught: %A" ex
            do! Deferred.unit
            invalidOp "Exception in exception handler!"

          counter <- counter + 1
      with ex ->
        printfn "Caught exception in exception handler: %A" ex
        do! afterMs 2000
      return ()
    })
  with ex ->
    printfn "Uncaught exception: %A" ex

  printfn "Dispatcher stopped."

let testUncaughtException () =
  try
    Dispatcher.run dispatcher (fun () -> deferred {
      invalidOp "This goes to the root supervisor."
    })
  with ex ->
    printfn "Uncaught exception: %A" ex

let testFor () =
  Dispatcher.run dispatcher (fun () -> deferred {
    for i in System.Linq.Enumerable.Range(1, 10) do
      do! afterMs 100
      for j in 1 .. 5 do
        printfn "(%d, %d)" i j
        do! afterMs 10
      do! Deferred.unit
  })

let wait ms = deferred {
  do! afterMs ms
  printfn "I waited %d ms." ms
}

let waitAndReturn ms value = deferred {
  do! afterMs ms
  printfn "I waited %d ms." ms
  return value
}

let testAll () =
  Dispatcher.run dispatcher (fun () -> deferred {
    do! Deferred.allUnit [wait 1000; wait 100; wait 200; wait 5]
    printfn "All done."
    let! values = Deferred.all [waitAndReturn 200 1; waitAndReturn 100 2; waitAndReturn 150 3; waitAndReturn 5 4]
    printfn "All done: %A" values
    let! values = Deferred.Array.all [|waitAndReturn 200 1; waitAndReturn 100 2; waitAndReturn 150 3; waitAndReturn 5 4|]
    printfn "All done: %A" values
    let! values = Deferred.Seq.all [|waitAndReturn 200 1; waitAndReturn 100 2; waitAndReturn 150 3; waitAndReturn 5 4|]
    printfn "All done: %A" values
  })

let testAny () =
  Dispatcher.run dispatcher (fun () -> deferred {
    let list = [never (); wait 1000; wait 100; wait 200]
    let! (i, ()) = Deferred.anyi list
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
      () --> v2
      () --> v1
    })
    do!
      choose
        [ choice v1 (fun () -> printfn "%s" "This should display");
          choice (afterMs 200) (fun () -> printfn "%s" "This should not display");
          choice v2 (fun () -> printfn "%s" "This should not display"); ]
  })

[<Struct>]
type MyStruct(x : int) =
  interface System.IDisposable with
    member this.Dispose() = printfn "Struct dispose!"

type MyClass(x : int) =
  interface System.IDisposable with
    member this.Dispose() = printfn "Class dispose!"

let testUsing () =
  try
    Dispatcher.run dispatcher (fun () -> deferred {
      printfn "Default:"
      using (new MyStruct(0)) (fun _ -> printfn "Struct default")
      using (new MyStruct(1)) (fun _ -> printfn "Struct non-default")
      using (new MyClass(0)) (fun _ -> printfn "Class")
      using null (fun _ -> printfn "null")

      printfn "Deferred:"
      use _ = new MyStruct(0)
      use _ = new MyStruct(1)
      use _ = new MyClass(0)
      use _ = null

      printfn "Done"
      invalidOp "Test exception"
    })
  with ex ->
    printfn "Caught exception: %A" ex

let testSequenceFunctions () =
  Dispatcher.run dispatcher (fun () -> deferred {
    let config = [(125, 1); (250, 2); (500, 3); (1000, 4); (10, 5); (20, 6); (5, 7)]
    let! values = config |> Deferred.List.map Parallelism.Sequential ((<||) waitAndReturn)
    printfn "List sequential: %A" values
    let! values = config |> Deferred.List.map Parallelism.Parallel ((<||) waitAndReturn)
    printfn "List parallel: %A" values

    let! three = config |> Deferred.List.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 5))
    printfn "List tryFind: %A" three
    let! nothing = config |> Deferred.List.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 99))
    printfn "List tryFind: %A" nothing

    let config = [|(125, 1); (250, 2); (500, 3); (1000, 4); (10, 5); (20, 6); (5, 7)|]
    let! values = config |> Deferred.Array.map Parallelism.Sequential ((<||) waitAndReturn)
    printfn "Array sequential: %A" values
    let! values = config |> Deferred.Array.map Parallelism.Parallel ((<||) waitAndReturn)
    printfn "Array parallel: %A" values

    let! three = config |> Deferred.Array.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 5))
    printfn "Array tryFind: %A" three
    let! nothing = config |> Deferred.Array.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 99))
    printfn "Array tryFind: %A" nothing

    let! three = (config |> Seq.skip 1) |> Deferred.Seq.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 5))
    printfn "Seq tryFind: %A" three
    let! nothing = (config |> Seq.skip 1) |> Deferred.Seq.tryFind (fun c -> c ||> waitAndReturn >>| ((=) 99))
    printfn "Seq tryFind: %A" nothing
  })

let testCycle () =
  Dispatcher.run dispatcher (fun () -> deferred {
    let v1 = Deferred.createVar ()
    let v2 = Deferred.createVar ()
    v1 >-- v2
    v2 >-- v1
    do! Deferred.anyUnit [v1; v2; afterMs 1000]
    printfn "Waited 1 second - detected cycle!"

    let v1 = Deferred.createVar ()
    let v2 = Deferred.createVar ()
    let v3 = Deferred.createVar ()
    v1 >-- v2
    v2 >-- v3
    v3 >-- v1
    do! Deferred.anyUnit [v1; v2; v3; afterMs 1000]
    printfn "Waited 1 second - detected cycle!"
  })

let testDSeqBuilder () =
  try
    Dispatcher.run dispatcher (fun () -> deferred {
      let xs = deferredSeq {
        yield "First"

        let! x = afterMs 1000 >>| fun () -> "Waited 1s"
        yield x

        printfn "YieldFrom:"

        yield! DeferredSeq.init 10 (fun i -> afterMs 100 >>| fun () -> string (i + 1))
        if x <> "Waited 1s NOT" then
          yield "This should be printed"

        printfn "while:"

        let mutable i = 1
        while (afterMs 50 >>| fun () -> i <= 10) do
          yield string i
          yield string (i + 10)
          i <- i + 1

        printfn "for:"

        for i in 1 .. 10 do
          yield string i
          yield string (i + 10)

          try
            try
              if i = 9 then
                invalidOp "Raise at i = 9"
            with ex ->
              printfn "Caught exception: %A" ex
              yield "Yield from exception handler"
          finally
            printfn "Executing finally"

        printfn "DSeq disposable:"
        use _ = new MyStruct(0)
        use _ = new MyStruct(1)
        use _ = new MyClass(0)
        use _ = null

        yield "Yielding just before raise"
        invalidOp "Test exception"

        printfn "Done"
      }

      for x in xs do
        printfn "%s" x
    })
  with ex ->
    printfn "Caught exception: %A" ex

let testPipe () =
  let r = new System.Random()
  Dispatcher.run dispatcher (fun () -> deferred {
    let primesReader = Pipe.createReader (fun writer -> deferred {
      let isPrime n =
        let limit = int (sqrt (double n))
        let rec loop i =
          if i > limit then
            true
          else if n % i = 0 then
            false
          else
            loop (i + 2)
        if n = 1 || n % 2 = 0 then
          false
        else
          loop 3
      for i = 1 to 200 do
        if isPrime i then
          do! Pipe.write writer (i, true)
          do! afterMs (r.Next(500))
        else
          Pipe.writeImmediately writer (i, false)
    })
    let printer id = deferred {
      let mutable count = 0
      do! primesReader |> Pipe.iterBatch (Pipe.BatchSize.AtMost 3) (fun xs ->
        xs |> Array.iter (fun (n, p) ->
          printfn "[%d]: %d (%s)" id n (if p then "PRIME" else "NOT PRIME")
          count <- count + 1
        )
        afterMs (r.Next(500))
      )
      printfn "[%d]: Printed %d elements." id count
    }
    do! Deferred.allUnit [printer 1; printer 2]
  })

let testOfAsync () =
  let a = async {
    invalidOp "Test exception"
  }
  Dispatcher.run dispatcher (fun () -> deferred {
    try
      do! Deferred.ofAsyncStart a
    with ex ->
      printfn "Caught exception: %A" ex
  })
