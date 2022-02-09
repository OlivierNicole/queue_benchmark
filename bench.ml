(* Benchmarks *****************************************************************)

let run_n (n : int) ~(thread : int -> 'a) ~(init : 'b) ~(collect : 'b -> 'a -> 'b) : 'b =
  let threads = Array.init n (fun i -> Domain.spawn (fun () -> thread i)) in
  Array.fold_left (fun acc th -> collect acc (Domain.join th)) init threads

let printf_lock = Queue.TTASLock.make ()
let my_fprintf out =
  Printf.ksprintf begin fun s ->
    Queue.TTASLock.acquire printf_lock ;
    Printf.fprintf out "%s" s ;
    Queue.TTASLock.release printf_lock ;
  end
let my_printf fmt = my_fprintf stdout fmt

let nmax = 1_200_000
let nb_consumers = 1
let nb_per_consumer =
  assert (nmax mod nb_consumers = 0) ;
  nmax / nb_consumers
let nb_producers = 1
let nb_per_producer =
  assert (nmax mod nb_producers = 0) ;
  nmax / nb_producers
let capacity = 16_384

module Bench (Q : Queue.QUEUE) = struct
  let channel = Q.make ~capacity ~dummy:42

  let producer i =
    let init = i*nb_per_producer in
    for n = init+1 to init+nb_per_producer do
      (*my_printf "enqueue %i…\n" n ;*)
      Q.enqueue channel n ;
      (*my_printf "enqueued\n" ;*)
    done

  let consumer () =
    let sum = ref 0 in
    for _ = 1 to nb_per_consumer do
      (*my_printf "    dequeue…\n" ;*)
      let n = Q.dequeue channel in
      sum := !sum + n ;
      (*my_printf "    dequeued %i\n" n ;*)
    done ;
    !sum

  let f () =
    let sum =
      run_n (nb_producers + nb_consumers)
        ~thread:(fun tid -> if tid < nb_producers then (producer tid ; 0) else consumer ())
        ~init:0
        ~collect:(+)
    in
    (*my_printf "sum = %i\n" sum ;*)
    assert (sum = nmax * (nmax + 1) / 2) ;
    (*my_printf "done\n"*)
    ()

  let bench ~name =
    let nruns = 100 in
    Printf.printf "=== %s ===\n" name;
    Printf.printf "#Producers: %i\n#Consumers: %i\nCapacity: %i\n%!" nb_producers
      nb_consumers capacity;
    let t0 = Unix.gettimeofday () in
    for _ = 1 to nruns do
      f ()
    done;
    let t1 = Unix.gettimeofday () in
    (*Printf.printf "Total %i runs: %f s\n" nruns (t1 -. t0);*)
    Printf.printf "Average time: %f ms\n\n" ((t1 -. t0) /. float_of_int nruns *. 1000.)
end

module B1 = Bench (Queue.BufferQueue)
module B2 = Bench (Queue.BufferQueueArrAtom)
module B3 = Bench (Queue.BufferQueueAtomArr)
module B4 = Bench (Queue.BufferQueueAtomArrOpt)

let orig = ref false
let arr_atom = ref false
let atom_arr = ref false
let atom_arr_opt = ref false

let speclist =
  [ ("-orig", Arg.Set orig, "Original queue implementation");
    ("-arratom", Arg.Set arr_atom, "Array of atomics");
    ("-atomarr", Arg.Set atom_arr, "Atomic arrays");
    ("-atomarropt", Arg.Set atom_arr_opt, "Atomic arrays (padded)");
  ]

let () =
  Arg.parse speclist
    (fun _ -> raise (Invalid_argument "anonymous argument not supported"))
    "bench [-orig] [-arratom] [-atomarr] [-atomarropt]";
  (if !atom_arr_opt then B4.bench ~name:"Atomic array (padded)");
  (if !orig then B1.bench ~name:"Original");
  (if !arr_atom then B2.bench ~name:"Array of atomics");
  (if !atom_arr then B3.bench ~name:"Atomic array");
