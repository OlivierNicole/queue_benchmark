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

let nmax = 40_000
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
    let init = tid*nb_per_producer in
    let bo = Queue.ExponentialBackoff.make () in
    for n = init+1 to init+nb_per_producer do
      (*my_printf "enqueue %i…\n" n ;*)
      Q.enqueue_noalloc q bo n;
      (*my_printf "enqueued\n" ;*)
    done;
    minor_allocated.(tid) <- minor_allocated.(tid) +. Gc.minor_words ()

  let consumer q tid =
    let sum = ref 0 in
    let tmp = ref 42 in
    let bo = Queue.ExponentialBackoff.make () in
    for _ = 1 to nb_per_consumer do
      (*my_printf "    dequeue…\n" ;*)
      let () = Q.dequeue_noalloc q tmp bo in
      Queue.ExponentialBackoff.reset bo;
      sum := !sum + !tmp ;
      (*my_printf "    dequeued %i\n" n ;*)
    done ;
    minor_allocated.(tid) <- minor_allocated.(tid) +. Gc.minor_words ();
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
end

module BOrig = Bench (Queue.BufferQueue)
module BArrAtom = Bench (Queue.BufferQueueArrAtom)
module BAtomArr = Bench (Queue.BufferQueueAtomArr)
module BAtomArrOpt = Bench (Queue.BufferQueueAtomArrOpt)

open Bechamel
open Toolkit

let mk_test ~name ~allocate f =
  Test.make_indexed_with_resource
    ~name
    (*~args:[ 64; 256; 1024; 4096; 16384; 32768 ]*)
    (*~args:[ 64; 1024; 16384; 32768 ]*)
    ~args:[ 16384; ]
    ~fmt:"%s:%05d"
    Test.uniq
    ~allocate
    ~free:(fun _ -> Gc.compact ())
    f

let test0 =
  mk_test
    ~name:"original"
    ~allocate:BOrig.allocate
    (fun _ -> Staged.stage BOrig.f)
(*
let test1 =
  mk_test
    ~name:"array of atomics"
    ~allocate:BArrAtom.allocate
    (fun _ -> Staged.stage BArrAtom.f)
let test2 =
  mk_test
    ~name:"atomic array"
    ~allocate:BAtomArr.allocate
    (fun _ -> Staged.stage BAtomArr.f)
*)
let test3 =
  mk_test
    ~name:"padded atomic array"
    ~allocate:BAtomArrOpt.allocate
    (fun _ -> Staged.stage BAtomArrOpt.f)
let test = Test.make_grouped ~name:"sum of integers" ~fmt:"%s (%s)"
  [ test0; test3 ]

module Producer_minor_allocated = struct
  type witness = unit
  let label _ = "producer-minor"
  let unit _ = "mnw"
  let make () = ()
  let load () = ()
  let unload () = ()
  let get () =
    StdLabels.Array.(fold_left ~f:(+.) ~init:0.
      (sub minor_allocated ~pos:0 ~len:nb_producers))
end

module Consumer_minor_allocated = struct
  type witness = unit
  let label _ = "consumer-minor"
  let unit _ = "mnw"
  let make () = ()
  let load () = ()
  let unload () = ()
  let get () =
    StdLabels.Array.(fold_left ~f:(+.) ~init:0.
      (sub minor_allocated ~pos:nb_producers ~len:nb_consumers))
end

module MyMeasures = struct
  let producer_minor_allocated =
    Measure.register (module Producer_minor_allocated)

  let consumer_minor_allocated =
    Measure.register (module Consumer_minor_allocated)
end

module MyInstance = struct
  let producer_minor_allocated =
    Measure.instance
      (module Producer_minor_allocated)
      MyMeasures.producer_minor_allocated

  let consumer_minor_allocated =
    Measure.instance
      (module Consumer_minor_allocated)
      MyMeasures.consumer_minor_allocated
end

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock;
      minor_collection; major_collection; ] @
    MyInstance.[ producer_minor_allocated; consumer_minor_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:100 ~quota:(Time.second 20.) ~stabilize:true ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v)) @@
      Instance.[ minor_allocated; major_allocated; monotonic_clock;
        minor_collection; major_collection; ] @
      MyInstance.[ producer_minor_allocated; consumer_minor_allocated ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w,h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
