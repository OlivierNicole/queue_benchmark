(*** Timer ********************************************************************)

module Time = struct

  (* nanoseconds since the beginning of the program *)
  type t = int

  let get () : t =
    int_of_float (Sys.time () *. 1e9)

  let pp out (t : t) =
    Printf.fprintf out "%.3gs" (float t /. 1.e9)

  let timed (f : unit -> 'a) : 'a =
    let t0 = get () in
    let res = f () in
    let t1 = get () in
    Printf.fprintf stderr "time: %a\n%!" pp (t1 - t0) ;
    res

end

(*** Backoff ******************************************************************)

module type BACKOFF = sig
  type t
  val make : ?min:float -> ?max:float -> unit -> t
  val backoff : t -> unit
  val reset : t -> unit
end

(* NOTE: This datastructure is not thread-safe. *)
module ExponentialBackoff : BACKOFF = struct
  type t = {
    min : float ;
    max : float ;
    mutable cur : float ;
  }
  (* 2^{−1074} is the smallest positive number that a float can represent.
   * 2^{-10} seconds is about a millisecond. *)
  let make ?(min=0x1.p-1074) ?(max=0x1.p-10) () =
    assert (0.0 <= min && min <= max) ;
    { min ; max ; cur = min }
  let backoff bo =
    let cur = bo.cur in
    bo.cur <- min bo.max (cur +. cur) ;
    (*Domain.Sync.cpu_relax () ;*) (* Obsolete and no-op on x86 *)
    Unix.sleepf (Random.float cur)
  let reset bo =
    bo.cur <- bo.min
end

(*** Lock *********************************************************************)

module type LOCK = sig
  type t
  val make : unit -> t
  val acquire : t -> unit
  val release : t -> unit
end

let make_critical (module Lock : LOCK) () : (unit -> 'a) -> 'a =
  let lock = Lock.make () in
  fun f ->
    Lock.acquire lock ;
    begin match f () with
    | res         -> Lock.release lock ; res
    | exception e -> Lock.release lock ; raise e
    end

let critical_timer : Time.t Atomic.t = Atomic.make 0

let make_critical_timed (module Lock : LOCK) () : (unit -> 'a) -> 'a =
  let lock = Lock.make () in
  fun f ->
    let t0 = Time.get () in
    Lock.acquire lock ;
    begin match f () with
    | res         ->
        Lock.release lock ;
        Atomic.fetch_and_add critical_timer (Time.get () - t0) |> ignore ;
        res
    | exception e ->
        Lock.release lock ;
        Atomic.fetch_and_add critical_timer (Time.get () - t0) |> ignore ;
        raise e
    end

(* the test-and-set lock:
 * the most basic spin lock, CPU-intensive and provokes many cache misses. *)
module TASLock : LOCK = struct
  type t = bool Atomic.t
  let make () =
    Atomic.make false
  let acquire lock =
    while Atomic.exchange lock true do () done
  let release lock =
    assert (Atomic.exchange lock false)
end

(* the test-and-test-and-set lock:
 * has a much better cache behavior with many threads. *)
module TTASLock : LOCK = struct
  type t = bool Atomic.t
  let make () =
    Atomic.make false
  let acquire lock =
    while Atomic.get lock do () done ;
    while Atomic.exchange lock true do
      while Atomic.get lock do () done
    done
  let release lock =
    assert (Atomic.exchange lock false)
end

(*** Queue ********************************************************************)

module type QUEUE = sig
  type 'a t
  val make : capacity:int -> dummy:'a -> 'a t
  val try_enqueue : 'a t -> 'a -> ExponentialBackoff.t -> bool
  val enqueue : 'a t -> 'a -> unit
  val enqueue_noalloc : 'a t -> ExponentialBackoff.t -> 'a -> unit
  val try_dequeue : 'a t -> 'a ref -> ExponentialBackoff.t -> bool
  val dequeue : 'a t -> 'a
  val dequeue_noalloc : 'a t -> 'a ref -> ExponentialBackoff.t -> unit
end

module BufferQueue : QUEUE = struct

  type 'a cell =
    {
      timestamp : int Atomic.t ;
      mutable element : 'a ;
    }

  type 'a t =
    {
      buf : 'a cell array ;
      mask : int ;
      head : int Atomic.t ;
      tail : int Atomic.t ;
      dummy : 'a;
    }

  (* Default recommended capacity: 64 *)
  let make ~capacity ~dummy =
    assert (capacity >= 2) ;
    (* check that the capacity is a power of two (not required by the algorithm,
     * but it allows more efficient array lookups): *)
    assert (capacity land (capacity - 1) = 0) ;
    {
      buf = Array.init capacity (fun i ->
          {
            timestamp = Atomic.make i ;
            element = dummy ;
          }
        ) ;
      mask = capacity - 1 ; (* i.e. 0b111111 if capacity = 0b1000000 *)
      head = Atomic.make 0 ;
      tail = Atomic.make 0 ;
      dummy ;
    }

  let try_enqueue q x bo =
    let rec try_enqueue () =
      let head = Atomic.get q.head in
      let cell = q.buf.(head land q.mask) in
      let ts = Atomic.get cell.timestamp in
      if ts = head && Atomic.compare_and_set q.head head (head+1) then begin
        (* cell is available and we got it first: write our value *)
        cell.element <- x ;
        Atomic.set cell.timestamp (head+1) ;
        true
      end
      else if ts < head then
        (* cell is still in use in previous round: fail here *)
        false
      else begin
        (* either ts > head, or the CAS failed; in either case,
        * another enqueuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_enqueue ()
      end
    in
    try_enqueue ()

  let enqueue_noalloc q bo x =
    while not (try_enqueue q x bo) do () done

  let enqueue q x =
    let bo = ExponentialBackoff.make () in
    while not @@ try_enqueue q x bo do () done

  let try_dequeue q ref bo =
    let rec try_dequeue () =
      let tail = Atomic.get q.tail in
      let cell = q.buf.(tail land q.mask) in
      let ts = Atomic.get cell.timestamp in
      if ts = tail+1 && Atomic.compare_and_set q.tail tail (tail+1) then begin
        (* cell is available and we got it first: read its value *)
        let x = cell.element in
        (*! cell.element <- dummy ; !*) (* TODO for garbage collection *)
        Atomic.set cell.timestamp (tail + q.mask+1) ;
        ref := x; true
      end
      else if ts < tail+1 then
        (* no element has been enqueued in this cell yet: fail here *)
        false
      else begin
        (* either ts > tail+1, or the CAS failed; in either case,
        * another dequeuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_dequeue ()
      end
    in
    try_dequeue ()

  let rec dequeue q =
    let bo = ExponentialBackoff.make () in
    let ref = ref q.dummy in
    if try_dequeue q ref bo then !ref else dequeue q

  let rec dequeue_noalloc q ref bo =
    if try_dequeue q ref bo then () else dequeue_noalloc q ref bo

end

module BufferQueueArrAtom : QUEUE = struct

  type 'a t =
    {
      timestamps : int Atomic.t array ;
      elements : 'a array ;
      mask : int ;
      head : int Atomic.t ;
      tail : int Atomic.t ;
      dummy : 'a ;
    }

  (* Default recommended capacity: 64 *)
  let make ~capacity ~dummy =
    assert (capacity >= 2) ;
    (* check that the capacity is a power of two (not required by the algorithm,
     * but it allows more efficient array lookups): *)
    assert (capacity land (capacity - 1) = 0) ;
    {
      timestamps = Array.init capacity (fun i -> Atomic.make i) ;
      elements = Array.make capacity dummy ;
      mask = capacity - 1 ; (* i.e. 0b111111 if capacity = 0b1000000 *)
      head = Atomic.make 0 ;
      tail = Atomic.make 0 ;
      dummy ;
    }

  let try_enqueue q x bo =
    let rec try_enqueue () =
      let head = Atomic.get q.head in
      let index = head land q.mask in
      let r_timestamp = q.timestamps.(index) in
      let ts = Atomic.get r_timestamp in
      if ts = head && Atomic.compare_and_set q.head head (head+1) then begin
        (* cell is available and we got it first: write our value *)
        q.elements.(index) <- x ;
        Atomic.set r_timestamp (head+1) ;
        true
      end
      else if ts < head then
        (* cell is still in use in previous round: fail here *)
        false
      else begin
        (* either ts > head, or the CAS failed; in either case,
        * another enqueuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_enqueue ()
      end
    in
    try_enqueue ()

  let enqueue_noalloc q bo x =
    while not @@ try_enqueue q x bo do () done

  let enqueue q x =
    let bo = ExponentialBackoff.make () in
    while not @@ try_enqueue q x bo do () done

  let try_dequeue q ref bo =
    let rec try_dequeue () =
      let tail = Atomic.get q.tail in
      let index = tail land q.mask in
      let r_timestamp = q.timestamps.(index) in
      let ts = Atomic.get r_timestamp in
      if ts = tail+1 && Atomic.compare_and_set q.tail tail (tail+1) then begin
        (* cell is available and we got it first: read its value *)
        let x = q.elements.(index) in
        (*! cell.element <- dummy ; !*) (* TODO for garbage collection *)
        Atomic.set r_timestamp (tail + q.mask+1) ;
        ref := x; true
      end
      else if ts < tail+1 then
        (* no element has been enqueued in this cell yet: fail here *)
        false
      else begin
        (* either ts > tail+1, or the CAS failed; in either case,
        * another dequeuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_dequeue ()
      end
    in
    try_dequeue ()

  let rec dequeue q =
    let bo = ExponentialBackoff.make () in
    let ref = ref q.dummy in
    if try_dequeue q ref bo then !ref else dequeue q

  let rec dequeue_noalloc q ref bo =
    if try_dequeue q ref bo then () else dequeue_noalloc q ref bo

end

(* [x] must be a positive power of 2. *)
let log2 =
  let rec aux acc x =
    if x = 1 then acc else aux (acc+1) (x lsr 1)
  in
  function x -> aux 0 x

module BufferQueueAtomArr : QUEUE = struct

  type 'a t =
    {
      timestamps : int array ;
      elements : 'a array ;
      mask : int ;
      head : int Atomic.t ;
      tail : int Atomic.t ;
      dummy : 'a ;
    }

  (* Default recommended capacity: 64 *)
  let make ~capacity ~dummy =
    assert (capacity >= 2) ;
    (* check that the capacity is a power of two (not required by the algorithm,
     * but it allows more efficient array lookups): *)
    assert (capacity land (capacity - 1) = 0) ;
    {
      timestamps = Array.init capacity (fun i -> i) ;
      elements = Array.make capacity dummy ;
      mask = capacity - 1 ; (* i.e. 0b111111 if capacity = 0b1000000 *)
      head = Atomic.make 0 ;
      tail = Atomic.make 0 ;
      dummy ;
    }

  let try_enqueue q x bo =
    let rec try_enqueue () =
      let head = Atomic.get q.head in
      let index = head land q.mask in
      let ts = Atomic.Array.get q.timestamps index in
      if ts = head && Atomic.compare_and_set q.head head (head+1) then begin
        (* cell is available and we got it first: write our value *)
        q.elements.(index) <- x ;
        Atomic.Array.set q.timestamps index (head+1) ;
        true
      end
      else if ts < head then
        (* cell is still in use in previous round: fail here *)
        false
      else begin
        (* either ts > head, or the CAS failed; in either case,
        * another enqueuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_enqueue ()
      end
    in
    try_enqueue ()

  let enqueue_noalloc q bo x =
    while not @@ try_enqueue q x bo do () done

  let enqueue q x =
    let bo = ExponentialBackoff.make () in
    while not @@ try_enqueue q x bo do () done

  let try_dequeue q ref bo =
    let rec try_dequeue () =
      let tail = Atomic.get q.tail in
      let index = tail land q.mask in
      let ts = Atomic.Array.get q.timestamps index in
      if ts = tail+1 && Atomic.compare_and_set q.tail tail (tail+1) then begin
        (* cell is available and we got it first: read its value *)
        let x = q.elements.(index) in
        (*! cell.element <- dummy ; !*) (* TODO for garbage collection *)
        Atomic.Array.set q.timestamps index (tail + q.mask+1) ;
        ref := x; true
      end
      else if ts < tail+1 then
        (* no element has been enqueued in this cell yet: fail here *)
        false
      else begin
        (* either ts > tail+1, or the CAS failed; in either case,
        * another dequeuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_dequeue ()
      end
    in
    try_dequeue ()

  let rec dequeue q =
    let bo = ExponentialBackoff.make () in
    let ref = ref q.dummy in
    if try_dequeue q ref bo then !ref else dequeue q

  let rec dequeue_noalloc q ref bo =
    if try_dequeue q ref bo then () else dequeue_noalloc q ref bo

end

module BufferQueueAtomArrOpt : QUEUE = struct

  type 'a t =
    {
      timestamps : int array ;
      elements : 'a array ;
      mask : int ;
      head : int Atomic.t ;
      tail : int Atomic.t ;
      dummy : 'a ;
    }

  (* Default recommended capacity: 64 *)
  let make ~capacity ~dummy =
    assert (capacity >= 2) ;
    (* check that the capacity is a power of two (not required by the algorithm,
     * but it allows more efficient array lookups): *)
    assert (capacity land (capacity - 1) = 0) ;
    {
      timestamps = Array.init (8*capacity)
        (fun i -> if i mod 8 = 0 then i / 8 else 42) ;
      elements = Array.make (8*capacity) dummy ;
      mask = capacity - 1 ; (* i.e. 0b111111 if capacity = 0b1000000 *)
      head = Atomic.make 0 ;
      tail = Atomic.make 0 ;
      dummy ;
    }

  let try_enqueue q x bo =
    let rec try_enqueue () =
      let head = Atomic.get q.head in
      let index = head land q.mask in
      let real_index = 8 * index in
      let ts = Atomic.Array.get q.timestamps real_index in
      if ts = head && Atomic.compare_and_set q.head head (head+1) then begin
        (* cell is available and we got it first: write our value *)
        q.elements.(real_index) <- x ;
        Atomic.Array.set q.timestamps real_index (head+1) ;
        true
      end
      else if ts < head then
        (* cell is still in use in previous round: fail here *)
        false
      else begin
        (* either ts > head, or the CAS failed; in either case,
        * another enqueuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_enqueue ()
      end
    in
    try_enqueue ()

  let enqueue_noalloc q bo x =
    while not (try_enqueue q x bo) do () done

  let enqueue q x =
    let bo = ExponentialBackoff.make () in
    while not (try_enqueue q x bo) do () done

  let try_dequeue q ref bo =
    let rec try_dequeue () =
      let tail = Atomic.get q.tail in
      let index = tail land q.mask in
      let real_index = 8 * index in
      let ts = Atomic.Array.get q.timestamps real_index in
      if ts = tail+1 && Atomic.compare_and_set q.tail tail (tail+1) then begin
        (* cell is available and we got it first: read its value *)
        let x = q.elements.(real_index) in
        (*! cell.element <- dummy ; !*) (* TODO for garbage collection *)
        Atomic.Array.set q.timestamps real_index (tail + q.mask+1) ;
        ref := x ; true
      end
      else if ts < tail+1 then
        (* no element has been enqueued in this cell yet: fail here *)
        false
      else begin
        (* either ts > tail+1, or the CAS failed; in either case,
        * another dequeuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_dequeue ()
      end
    in
    try_dequeue ()

  let rec dequeue q =
    let bo = ExponentialBackoff.make () in
    let ref = ref q.dummy in
    if try_dequeue q ref bo then !ref else dequeue q

  let rec dequeue_noalloc q ref bo =
    if try_dequeue q ref bo then () else dequeue_noalloc q ref bo

end

module BufferQueuePadHeadTail : QUEUE = struct

  type 'a cell =
    {
      timestamp : int Atomic.t ;
      mutable element : 'a ;
    }

  type 'a t =
    {
      buf : 'a cell array ;
      mask : int ;
      (* The head is at index 0, the tail at index 8, the rest is padding so
         that the two are on different cache lines. *)
      headtail : int array;
      dummy : 'a;
    }

  (* Default recommended capacity: 64 *)
  let make ~capacity ~dummy =
    assert (capacity >= 2) ;
    (* check that the capacity is a power of two (not required by the algorithm,
     * but it allows more efficient array lookups): *)
    assert (capacity land (capacity - 1) = 0) ;
    {
      buf = Array.init capacity (fun i ->
          {
            timestamp = Atomic.make i ;
            element = dummy ;
          }
        ) ;
      mask = capacity - 1 ; (* i.e. 0b111111 if capacity = 0b1000000 *)
      headtail = Array.make 9 0;
      dummy ;
    }

  let try_enqueue q x bo =
    let rec try_enqueue () =
      let head = Atomic.Array.get q.headtail 0 in
      let cell = q.buf.(head land q.mask) in
      let ts = Atomic.get cell.timestamp in
      if ts = head && Atomic.Array.compare_and_set q.headtail 0 head (head+1) then begin
        (* cell is available and we got it first: write our value *)
        cell.element <- x ;
        Atomic.set cell.timestamp (head+1) ;
        true
      end
      else if ts < head then
        (* cell is still in use in previous round: fail here *)
        false
      else begin
        (* either ts > head, or the CAS failed; in either case,
        * another enqueuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_enqueue ()
      end
    in
    try_enqueue ()

  let enqueue_noalloc q bo x =
    while not (try_enqueue q x bo) do () done

  let enqueue q x =
    let bo = ExponentialBackoff.make () in
    while not @@ try_enqueue q x bo do () done

  let try_dequeue q ref bo =
    let rec try_dequeue () =
      let tail = Atomic.Array.get q.headtail 8 in
      let cell = q.buf.(tail land q.mask) in
      let ts = Atomic.get cell.timestamp in
      if ts = tail+1 && Atomic.Array.compare_and_set q.headtail 8 tail (tail+1) then begin
        (* cell is available and we got it first: read its value *)
        let x = cell.element in
        (*! cell.element <- dummy ; !*) (* TODO for garbage collection *)
        Atomic.set cell.timestamp (tail + q.mask+1) ;
        ref := x; true
      end
      else if ts < tail+1 then
        (* no element has been enqueued in this cell yet: fail here *)
        false
      else begin
        (* either ts > tail+1, or the CAS failed; in either case,
        * another dequeuer got the cell before us: we try again *)
        ExponentialBackoff.backoff bo ;
        try_dequeue ()
      end
    in
    try_dequeue ()

  let rec dequeue q =
    let bo = ExponentialBackoff.make () in
    let ref = ref q.dummy in
    if try_dequeue q ref bo then !ref else dequeue q

  let rec dequeue_noalloc q ref bo =
    if try_dequeue q ref bo then () else dequeue_noalloc q ref bo

end

