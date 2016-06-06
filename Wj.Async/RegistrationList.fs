namespace Wj.Async

open System.Collections.Generic

module RegistrationList =
  type 'a State =
    | Initial
    // Invariant: rest must be Initial or Singly. This ensures that register is amortized O(1).
    // This invariant exists only for performance reasons, and can be removed at any time.
    | Singly of value : 'a * rest : 'a State
    | Doubly of 'a LinkedList

  type 'a T = {mutable state : 'a State}

  let create () = {state = Initial}

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

  let add t (x : 'a) =
    match t.state with
    | Doubly linkedList -> linkedList.AddFirst(x) |> ignore
    | _ -> t.state <- Singly (x, t.state)

  let register t (x : 'a) =
    let linkedList = ensureDoubly t
    Registration.fromLinkedListNode (linkedList.AddFirst(x))

  let moveFrom t from =
    let swap () =
      let temp = t.state
      t.state <- from.state
      from.state <- t.state
    match t.state with
    | Initial -> swap ()
    | Singly _ ->
      let finishMoveSingly reversed replacement =
        let rec reverseIntoSingly acc xs =
          match xs with
          | [] -> acc
          | x :: rest -> reverseIntoSingly (Singly (x, acc)) rest
        t.state <- reverseIntoSingly t.state reversed
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

  let clear t = t.state <- Initial

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
