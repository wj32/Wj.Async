namespace Wj.Async

open System.Collections.Generic

module RegistrationList =
  type 'a State =
    | Initial
    // Invariant: rest must be Initial or Singly. This ensures that register is amortized O(1).
    // This invariant exists only for performance reasons, and can be removed at any time.
    | Singly of value : 'a * rest : 'a State
    | Doubly of linkedList : 'a LinkedList

  type 'a T = {mutable state : 'a State; mutable emptiedCallbacks : (unit -> unit) list}

  let create () = {state = Initial; emptiedCallbacks = []}

  let moveToFromLinkedList (toLinkedList : 'a LinkedList) (fromLinkedList : 'a LinkedList) after start =
    let rec loop (after : _ LinkedListNode) (node : _ LinkedListNode) =
      match node with
      | null -> ()
      | _ ->
        let next = node.Next
        fromLinkedList.Remove(node)
        toLinkedList.AddAfter(after, node)
        loop node next
    loop after start

  let moveToLinkedListFromState (toLinkedList : 'a LinkedList) after state =
    let rec loop (after : _ LinkedListNode) (state : 'a State) =
      match state with
      | Initial -> ()
      | Singly (x, rest : 'a State) -> loop (toLinkedList.AddAfter(after, x)) rest
      | Doubly fromLinkedList -> moveToFromLinkedList toLinkedList fromLinkedList after fromLinkedList.First
    loop after state

  let moveToStartOfLinkedListFromState (toLinkedList : 'a LinkedList) (state : 'a State) =
    match state with
    | Initial -> ()
    | Singly (x, rest) -> moveToLinkedListFromState toLinkedList (toLinkedList.AddFirst(x)) rest
    | Doubly fromLinkedList ->
      match fromLinkedList.First with
      | null -> ()
      | first ->
        let next = first.Next
        fromLinkedList.Remove(first)
        toLinkedList.AddFirst(first)
        moveToFromLinkedList toLinkedList fromLinkedList first next

  let ensureDoubly t =
    match t.state with
    | Doubly linkedList -> linkedList
    | _ ->
      let linkedList = new LinkedList<'a>()
      moveToStartOfLinkedListFromState linkedList t.state
      t.state <- Doubly linkedList
      linkedList

  let isEmpty t =
    match t.state with
    | Initial -> true
    | Singly _ -> false
    | Doubly linkedList -> linkedList.Count = 0

  let checkEmpty t =
    if isEmpty t then
      t.emptiedCallbacks |> List.iter (fun f -> f ())
      t.emptiedCallbacks <- []

  let addEmptiedCallback t f =
    if isEmpty t then
      f ()
    else
      t.emptiedCallbacks <- f :: t.emptiedCallbacks

  let add t (x : 'a) =
    match t.state with
    | Doubly linkedList -> linkedList.AddFirst(x) |> ignore
    | _ -> t.state <- Singly (x, t.state)

  let register t (x : 'a) =
    let linkedList = ensureDoubly t
    let mutable node = linkedList.AddFirst(x)
    let mutable captured = t
    { new IRegistration with
        member this.Remove() =
          match node with
          | null -> ()
          | _ ->
            node.List.Remove(node)
            node <- null
            checkEmpty captured
            captured <- Unchecked.defaultof<'a T> // Fast way of getting rid of our reference to T
    }

  let moveFrom t from =
    let swap () =
      let temp = t.state
      t.state <- from.state
      from.state <- t.state
    match t.state with
    | Initial -> swap ()
    | Singly _ ->
      let finishMoveSingly reversed replacement =
        t.state <- reversed |> List.fold (fun acc x -> Singly (x, acc)) t.state
        from.state <- replacement
      let rec attemptMoveSingly acc state =
        match state with
        | Initial -> finishMoveSingly acc state
        | Singly (x, rest) -> attemptMoveSingly (x :: acc) rest
        | Doubly fromLinkedList ->
          match fromLinkedList.First with
          | null -> finishMoveSingly acc state
          | _ ->
            // Failure: the source contains a nonempty doubly linked list, so abort and convert the
            // destination to a doubly linked list.
            let linkedList = ensureDoubly t
            moveToStartOfLinkedListFromState linkedList from.state
      attemptMoveSingly [] from.state
    | Doubly toLinkedList ->
      match toLinkedList.First with
      | null -> swap ()
      | _ -> moveToStartOfLinkedListFromState toLinkedList from.state
    checkEmpty from

  let clear t = t.state <- Initial; checkEmpty t

  let ofList list =
    {state = List.foldBack (fun x acc -> Singly (x, acc)) list Initial; emptiedCallbacks = []}

  let toList t =
    let rec loop acc state =
      match state with
      | Initial -> acc
      | Singly (x, rest) -> loop (x :: acc) rest
      | Doubly linkedList ->
        let rec loop acc (node : _ LinkedListNode) =
          match node with
          | null -> acc
          | _ -> loop (node.Value :: acc) node.Next
        loop acc linkedList.First
    loop [] t.state
