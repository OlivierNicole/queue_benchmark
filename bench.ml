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

(*! module Queue = UnboundedQueue !*)
(*module Queue = UnboundedQueue2*)
module Queue = Queue.BufferQueueAtomArrOpt


(** Functional correctness test *)

let nmax = 1_200_000
let nb_consumers = 1
let nb_per_consumer =
  assert (nmax mod nb_consumers = 0) ;
  nmax / nb_consumers
let nb_producers = 1
let nb_per_producer =
  assert (nmax mod nb_producers = 0) ;
  nmax / nb_producers
let channel = Queue.make ~capacity:8192 ~dummy:42

let producer i =
  let init = i*nb_per_producer in
  for n = init+1 to init+nb_per_producer do
    (*my_printf "enqueue %i…\n" n ;*)
    Queue.enqueue channel n ;
    (*my_printf "enqueued\n" ;*)
  done

let consumer () =
  let sum = ref 0 in
  for _ = 1 to nb_per_consumer do
    (*my_printf "    dequeue…\n" ;*)
    let n = Queue.dequeue channel in
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

let () =
  let nruns = 100 in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to nruns do
    f ()
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "#Producers: %i\n#Consumers: %i\n" nb_producers nb_consumers;
  (*Printf.printf "Total %i runs: %f s\n" nruns (t1 -. t0);*)
  Printf.printf "Average time: %f ms\n" ((t1 -. t0) /. float_of_int nruns *. 1000.)

let () =
  exit 0


(*
(** Performance test *)

let nmax = 100_000
let nb_consumers = 10
let channel = Queue.make ~capacity:4 ~dummy:(42,None)

let producer () =
  let primes = ref [] in
  for n = 1 to nmax do
    let elt =
      begin match List.find (fun p -> n mod p = 0) !primes with
      | p                   -> (n, Some p)
      | exception Not_found -> if n <> 1 then primes := n :: !primes; (n, None)
      end
    in
    Queue.enqueue channel elt
  done

let consumer () =
  let primes = ref [] in
  for _ = 1 to nmax / nb_consumers do
    let (n, opt_p) = Queue.dequeue channel in
    (*let s =
      begin match opt_p with
      | None   -> Printf.sprintf "%u is prime\n" n
      | Some p -> Printf.sprintf "%u is dividible by %u\n" n p
      end
    in
    Printf.printf "%s" s*)
    begin match opt_p with
    | None   -> primes := n :: !primes
    | Some _ -> ()
    end
  done ;
  List.rev !primes

(*
let () =
  let finished = Atomic.make false in
  Domain.spawn begin fun () ->
    while not @@ Atomic.get finished do
      Unix.sleep 1 ;
      Printf.printf "%!" ;
    done
  end ;
  run_n (1 + nb_consumers)
    ~thread:(fun tid -> if tid = 0 then producer () else consumer ())
    ~init:()
    ~collect:(fun () () -> ()) ;
  Atomic.set finished true ;
  Printf.printf "done\n%!"
*)

let f () =
  (*
  let finished = Atomic.make false in
  ignore @@ Domain.spawn begin fun () ->
    while not @@ Atomic.get finished do
      Unix.sleep 1 ;
      Printf.printf "%!" ;
    done
  end ;
  *)
  let primes =
    run_n (1 + nb_consumers)
      ~thread:(fun tid -> if tid = 0 then (producer () ; []) else consumer ())
      ~init:[]
      ~collect:(@)
  in
  ignore (Sys.opaque_identity primes) ;
  (*
  Printf.printf "[";
  List.iter (fun i -> Printf.printf "%i, " i) primes;
  Printf.printf "]\n";
  *)
  (*Atomic.set finished true ;*)
  (*Printf.printf "done\n%!"*)
  ()

let () =
  let nruns = 1 in
  let t0 = Unix.gettimeofday () in
  for _ = 0 to nruns do
    f ()
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Total %i runs: %f s\n" nruns (t1 -. t0);
  Printf.printf "Average: %f s\n" ((t1 -. t0) /. float_of_int nruns);
*)
