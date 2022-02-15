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
    let bo = Queue.ExponentialBackoff.make () in
    for n = init+1 to init+nb_per_producer do
      (*my_printf "enqueue %i…\n" n ;*)
      Q.enqueue_noalloc q bo n;
      (*my_printf "enqueued\n" ;*)
    done;
    let minor1 = Gc.minor_words () in
    minor_allocated.(tid) <- minor_allocated.(tid) +. (minor1 -. minor0)

  let consumer q tid =
    let minor0 = Gc.minor_words () in
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

let capacity = 1024

let () =
  Arg.parse speclist
    (fun _ -> raise (Invalid_argument "anonymous argument not supported"))
    "bench [-orig] [-arratom] [-atomarr] [-atomarropt]";
  (if !orig then BOrig.bench ~name:"Original" ~capacity);
  (if !arr_atom then BArrAtom.bench ~name:"Array of atomics" ~capacity);
  (if !atom_arr then BAtomArr.bench ~name:"Atomic array" ~capacity);
  (if !atom_arr_opt then BAtomArrOpt.bench ~name:"Atomic array (padded)" ~capacity);

(* Fun fact, this generative functor is never instantiated and therefore has no
   observable effect... except that removing it makes implementation [original]
   run more than twice as fast. *)
module DoBechamelBench () = struct
  open Bechamel
  open Toolkit

  let mk_test ~name ~allocate f =
    Test.make_indexed_with_resource
      ~name
      (*~args:[ 64; 256; 1024; 4096; 16384; 32768 ]*)
      ~args:[ 64; 1024; 16384; 32768 ]
      (*~args:[ 16384; ]*)
      ~fmt:"%s:%05d"
      Test.uniq
      ~allocate
      ~free:(fun _ -> Gc.compact ())
      f

  let test0 =
    mk_test
      ~name:"orig"
      ~allocate:BOrig.allocate
      (fun _ -> Staged.stage BOrig.f)
  let test1 =
    mk_test
      ~name:"boxed arr."
      ~allocate:BArrAtom.allocate
      (fun _ -> Staged.stage BArrAtom.f)
  let test2 =
    mk_test
      ~name:"flat arr."
      ~allocate:BAtomArr.allocate
      (fun _ -> Staged.stage BAtomArr.f)
  let test3 =
    mk_test
      ~name:"padded flat arr."
      ~allocate:BAtomArrOpt.allocate
      (fun _ -> Staged.stage BAtomArrOpt.f)
  let test = Test.make_grouped ~name:"sum" ~fmt:"%s (%s)"
    [ test0; test1; test2; test3 ]

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

  module Monotonic_clock_ms = struct
    include Toolkit.Monotonic_clock

    let unit _ = "ms"
    let get () = get () /. 1_000_000.
  end

  module MyMeasures = struct
    let producer_minor_allocated =
      Measure.register (module Producer_minor_allocated)

    let consumer_minor_allocated =
      Measure.register (module Consumer_minor_allocated)

    let monotonic_clock_ms =
      Measure.register (module Monotonic_clock_ms)
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

    let monotonic_clock_ms =
      Measure.instance
        (module Monotonic_clock_ms)
        MyMeasures.monotonic_clock_ms
  end

  let instances =
    Instance.[ minor_allocated; major_allocated; minor_collection;
      major_collection; ] @
    MyInstance.[ monotonic_clock_ms; ]

  let benchmark () =
    let ols =
      Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
    in
    let cfg =
      Benchmark.cfg ~limit:2 ~quota:(Time.second 40.) ~stabilize:true ()
    in
    let raw_results = Benchmark.all cfg instances test in
    let results =
      List.map (fun instance -> Analyze.all ols instance raw_results) instances
    in
    let results = Analyze.merge ols instances results in
    (results, raw_results)

  let () =
    List.iter
      (fun v -> Bechamel_notty.Unit.add v (Measure.unit v)) instances

  let img (window, results) =
    Bechamel_notty.Multiple.image_of_ols_results ~rect:window
      ~predictor:Measure.run results

  open Notty_unix

  let results_files =
    instances |> List.map
      (fun instance ->
        let label = Measure.label instance in
        (label, Printf.sprintf "results-%s.json" label))

  let () =
    let window =
      match winsize Unix.stdout with
      | Some (w,h) -> { Bechamel_notty.w; h }
      | None -> { Bechamel_notty.w = 80; h = 1 }
    in
    let results, raw_results = benchmark () in
    raw_results |> Hashtbl.iter (fun name raw ->
      Printf.printf
        "%s ->\n  { samples = %d;\n    time = %f; }\n"
        name
        raw.Benchmark.stats.samples
        (Int64.to_float (Time.span_to_uint64_ns raw.Benchmark.stats.time) /. 1.e9));
    Printf.printf "Graphs printed to results-*.json\n\n";
    img (window, results) |> eol |> output_image;
    results_files |> List.iter
      (fun (y_label, results_file) ->
        let results =
          let open Bechamel_js in
          emit ~dst:(channel results_file) (fun _ -> Ok ()) ~compare
            ~x_label:Measure.run ~y_label (results, raw_results)
        in
        match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
      )
end
