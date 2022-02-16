(* Benchmarks *****************************************************************)

let run_n (n : int) ~(thread : int -> 'a) ~(init : 'b) ~(collect : 'b -> 'a -> 'b) : 'b =
  let threads = Array.init n (fun i -> Domain.spawn (fun () -> thread i)) in
  Array.fold_left (fun acc th -> collect acc (Domain.join th)) init threads

let nmax = 120_000_000
let nb_consumers = 1
let nb_per_consumer =
  assert (nmax mod nb_consumers = 0) ;
  nmax / nb_consumers
let nb_producers = 1
let nb_per_producer =
  assert (nmax mod nb_producers = 0) ;
  nmax / nb_producers

let minor_allocated : float array =
  Array.make (nb_producers + nb_consumers) 42. (* dummy value for debugging *)

module Bench (Q : Queue.QUEUE) = struct
  let allocate capacity = Q.make ~capacity ~dummy:42

  let producer q tid =
    let minor0 = Gc.minor_words () in
    let init = tid*nb_per_producer in
    for n = init+1 to init+nb_per_producer do
      (*my_printf "enqueue %i…\n" n ;*)
      Q.enqueue q n;
      (*my_printf "enqueued\n" ;*)
    done;
    let minor1 = Gc.minor_words () in
    minor_allocated.(tid) <- minor_allocated.(tid) +. (minor1 -. minor0)

  let consumer q tid =
    let minor0 = Gc.minor_words () in
    let sum = ref 0 in
    for _ = 1 to nb_per_consumer do
      (*my_printf "    dequeue…\n" ;*)
      let n = Q.dequeue q in
      sum := !sum + n ;
      (*my_printf "    dequeued %i\n" n ;*)
    done ;
    let minor1 = Gc.minor_words () in
    minor_allocated.(tid) <- minor_allocated.(tid) +. (minor1 -. minor0);
    !sum

  let f q =
    let sum =
      run_n (nb_producers + nb_consumers)
        ~thread:(fun tid -> if tid < nb_producers then (producer q tid ; 0) else consumer q tid)
        ~init:0
        ~collect:(+)
    in
    (*my_printf "sum = %i\n" sum ;*)
    assert (sum = nmax * (nmax + 1) / 2) ;
    (*my_printf "done\n"*)
    ()

  let bench ~name ~capacity =
    Gc.full_major (); (* avoid GC noise if possible??? *)
    let q = allocate capacity in
    let t0 = Unix.gettimeofday () in
    f q;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%s:%d: %f s\n" name capacity (t1 -. t0);
    let gc_stat = Gc.quick_stat () in
    Printf.printf
      "major collections: %d, minor collections: %d, minor allocated: %f\n\n"
      gc_stat.Gc.major_collections
      gc_stat.Gc.minor_collections
      gc_stat.Gc.minor_words
end

module BArrAtom = Bench (Queue.BufferQueueArrAtom)
module BAtomArr = Bench (Queue.BufferQueueAtomArr)
module BAtomArrOpt = Bench (Queue.BufferQueueAtomArrOpt)
module BOrig = Bench (Queue.BufferQueue)
module BOrigOpt = Bench (Queue.BufferQueuePadHeadTail)

let orig = ref false
let arr_atom = ref false
let atom_arr = ref false
let atom_arr_opt = ref false
let orig_opt = ref false

let speclist =
  [ ("-orig", Arg.Set orig, "Original queue implementation");
    ("-arratom", Arg.Set arr_atom, "Array of atomics");
    ("-atomarr", Arg.Set atom_arr, "Atomic arrays");
    ("-atomarropt", Arg.Set atom_arr_opt, "Atomic arrays (padded)");
    ("-origopt", Arg.Set orig_opt, "Original but padded head and tail");
  ]

let capacity = 1024

let () =
  Arg.parse speclist
    (fun _ -> raise (Invalid_argument "anonymous argument not supported"))
    "bench [-orig] [-arratom] [-atomarr] [-atomarropt]";
  (if !orig then BOrig.bench ~name:"Original" ~capacity);
  (if !arr_atom then BArrAtom.bench ~name:"Array of atomics" ~capacity);
  (if !atom_arr then BAtomArr.bench ~name:"Atomic array" ~capacity);
  (if !atom_arr_opt then BAtomArrOpt.bench ~name:"Atomic array (padded)" ~capacity);
  (if !orig_opt then BOrigOpt.bench ~name:"Original but padded head and tail" ~capacity);
