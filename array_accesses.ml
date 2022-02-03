let run_n (n : int) ~(thread : int -> 'a) ~(init : 'b) ~(collect : 'b -> 'a -> 'b) : 'b =
  let threads = Array.init n (fun i -> Domain.spawn (fun () -> thread i)) in
  Array.fold_left (fun acc th -> collect acc (Domain.join th)) init threads

let bench ~name (f : unit -> unit) =
  let nruns = 1000 in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to nruns do
    f ()
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "\n== Results for %s ==\n" name;
  Printf.printf "Total %i runs: %f s\n" nruns (t1 -. t0);
  Printf.printf "Average time: %f ms\n%!"
    ((t1 -. t0) /. float_of_int nruns *. 1000.)

let nb_threads = 8

let bench_parallel ~name (f : unit -> unit) : unit =
  let f' () =
    run_n nb_threads
      ~thread:(fun _ -> f ()) ~init:()
      ~collect:(fun _ _ -> ())
  in
  bench ~name f'

let roughly_l1_byte_size = 96_000
let roughly_l1_word_size = roughly_l1_byte_size / 8
let roughly_l2_byte_size = 1_280_000
let roughly_l2_word_size = roughly_l2_byte_size / 8

let nreads = 1_000_000

let access_flat_array array length =
  let rand = Random.State.make_self_init () in
  for _ = 1 to nreads do
    let i = Random.State.int rand length in
    let x = Atomic.Array.fetch_and_add array i 42 in
    ignore (Sys.opaque_identity x)
  done

let access_boxed_array array length =
  let rand = Random.State.make_self_init () in
  for _ = 1 to nreads do
    let i = Random.State.int rand length in
    let x = Atomic.fetch_and_add array.(i) 42 in
    ignore (Sys.opaque_identity x)
  done

let array : int array = Array.make roughly_l2_word_size 42

let () = bench_parallel ~name:"Parallel flat accesses" (fun () -> access_flat_array array roughly_l2_word_size)

let array_boxed : int Atomic.t array =
  Array.make roughly_l2_word_size (Atomic.make 42)

let () = bench_parallel ~name:"Parallel boxed accesses" (fun () -> access_boxed_array array_boxed roughly_l2_word_size)

let length_small = 1000

let array_small : int array = Array.make length_small 42

let () = bench_parallel ~name:"Parallel flat accesses (small)" (fun () -> access_flat_array array_small length_small)

let array_boxed_small : int Atomic.t array =
  Array.make length_small (Atomic.make 42)

let () = bench_parallel ~name:"Parallel boxed accesses (small)" (fun () -> access_boxed_array array_boxed_small length_small)
