namespace Wj.Async

module Queue =
  let [<Literal>] QueueEmpty = "The queue is empty."
  let [<Literal>] LengthExceedsElements = "The specified length exceeds the number of elements in the queue."

  type 'a T =
    { mutable data : 'a array;
      mutable head : int;
      mutable tail : int;
      mutable length : int; }

  let inline createWithData data index length =
    { data = data;
      head = index;
      tail = index + length;
      length = length; }

  let inline createFullWithData data = createWithData data 0 data.Length

  let inline createEmptyWithData data = createWithData data 0 0

  let create () = createEmptyWithData (Array.zeroCreate 2)

  let createWithCapacity capacity = createEmptyWithData (Array.zeroCreate capacity)

  let copyTo t dest length =
    assert (length <= t.length)
    if t.head < t.tail then
      Array.blit t.data t.head dest 0 length
    else if t.length <> 0 then
      let endLength = t.data.Length - t.head
      if length <= endLength then
        Array.blit t.data t.head dest 0 length
      else
        Array.blit t.data t.head dest 0 endLength
        Array.blit t.data 0 dest endLength (length - endLength)

  let ensureCapacity t requiredCapacity =
    if t.data.Length < requiredCapacity then
      let newCapacity = max (t.data.Length * 2) requiredCapacity
      let newData = Array.zeroCreate newCapacity
      copyTo t newData t.length
      t.data <- newData
      t.head <- 0
      t.tail <- t.length

  let enqueue t x =
    ensureCapacity t (t.length + 1)
    t.data.[t.tail] <- x
    t.tail <- (t.tail + 1) % t.data.Length
    t.length <- t.length + 1

  let enqueue' t (xs : _ array) =
    ensureCapacity t (t.length + xs.Length)
    let endLength = t.data.Length - t.tail
    if xs.Length <= endLength then
      Array.blit xs 0 t.data t.tail xs.Length
    else
      Array.blit xs 0 t.data t.tail endLength
      Array.blit xs endLength t.data 0 (xs.Length - endLength)
    t.tail <- (t.tail + xs.Length) % t.data.Length
    t.length <- t.length + xs.Length

  let inline dequeueGeneric t none some =
    if t.length = 0 then
      none ()
    else
      let x = t.data.[t.head]
      t.head <- (t.head + 1) % t.data.Length
      t.length <- t.length - 1
      some x

  let dequeue t = dequeueGeneric t (fun () -> invalidOp QueueEmpty) id

  let dequeue' t maxLength =
    let length = min maxLength t.length
    let xs = Array.zeroCreate length
    copyTo t xs length
    t.head <- (t.head + length) % t.data.Length
    t.length <- t.length - length
    xs

  let tryDequeue t = dequeueGeneric t (fun () -> None) Some

  let clear t =
    t.head <- 0
    t.tail <- 0
    t.length <- 0

  let inline firstGeneric t none some =
    if t.length = 0 then
      none ()
    else
      some t.data.[t.head]

  let first t = firstGeneric t (fun () -> invalidOp QueueEmpty) id

  let tryFirst t = firstGeneric t (fun () -> None) Some

  let inline lastGeneric t none some =
    if t.length = 0 then
      none ()
    else
      some t.data.[(t.tail - 1 + t.data.Length) % t.data.Length]

  let last t = lastGeneric t (fun () -> invalidOp QueueEmpty) id

  let tryLast t = lastGeneric t (fun () -> None) Some

  let length t = t.length

  let isEmpty t = t.length = 0

  let ofArray xs = createFullWithData (Array.copy xs)

  let toArray t =
    let xs = Array.zeroCreate t.length
    copyTo t xs t.length
    xs

  let ofList xs = createFullWithData (List.toArray xs)

  let toList t =
    if t.length = 0 then
      []
    else
      let rec loop acc index =
        let index' = (index - 1 + t.data.Length) % t.data.Length
        let acc' = t.data.[index'] :: acc
        if index' = t.head then
          acc'
        else
          loop acc' index'
      loop [] t.tail

  let ofSeq xs = createFullWithData (Seq.toArray xs)

  let toSeq t =
    if t.length = 0 then
      Seq.empty
    else
      seq {
        yield t.data.[t.head]
        let mutable index = (t.head + 1) % t.data.Length
        while index <> t.tail do
          yield t.data.[index]
          index <- (index + 1) % t.data.Length
      }

  let inline foldInline f state t =
    if t.length = 0 then
      state
    else
      let rec loop state index =
        let state' = f state t.data.[index]
        let index' = (index + 1) % t.data.Length
        if index' = t.tail then
          state'
        else
          loop state' index'
      loop state t.head

  let fold f state t = foldInline f state t

  let iter f t = t |> foldInline (fun () x -> f x) ()

  let inline createIter f t =
    let data = Array.zeroCreate t.length
    let length = t |> foldInline (fun i x -> f data i x) 0
    createWithData data 0 length

  let map f t = t |> createIter (fun data i x -> data.[i] <- f x; i + 1)

  let choose f t =
    t |> createIter (fun data i x -> match f x with Some y -> data.[i] <- y; i + 1 | None -> i)

  let filter f t = t |> createIter (fun data i x -> if f x then data.[i] <- x; i + 1 else i)
