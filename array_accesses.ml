let iterations = 1_000

let array_sizes = Array.init 15 (fun i -> 32 lsl i)

let atomic_but_allocate_junk nwords_junk (root : int list list ref) =
  let atomic = Atomic.make 0 in
  let junk = List.init (nwords_junk / 2) (fun _ -> 42) in
  ignore (Sys.opaque_identity junk);
  root := junk :: !root;
  atomic

let run_timed s arr_size f =
  Gc.full_major ();
  let start = Sys.time () in
  f arr_size;
  let duration = Sys.time() -. start in
  let duration_micros = duration *. 1_000_000. in
  let duration_millis = duration *. 1_000. in
  let duration_per_element = duration_micros /. (float_of_int arr_size) in
  Printf.printf "%s\t%d\t%11.3f\t%d\n%!" s arr_size duration_millis (int_of_float duration_per_element)

(*
let flat_sequential_read arr_size =
  let arr = Array.make arr_size 0 in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let x = Atomic.Array.get arr i in
        Sys.opaque_identity(x) |> ignore
    done
  done

let boxed_sequential_read arr_size =
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let x = Array.get arr i |> Atomic.get in
        Sys.opaque_identity(x) |> ignore
    done
  done

let flat_random_read arr_size =
  let arr = Array.make arr_size 0 in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let idx = Array.get arr_idx i in
      let x = Atomic.Array.get arr idx in
        Sys.opaque_identity(x) |> ignore
    done
  done

let boxed_random_read arr_size =
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let idx = Array.get arr_idx i in
      let x = Array.get arr idx |> Atomic.get in
        Sys.opaque_identity(x) |> ignore
    done
  done
*)

let boxed_subsequent_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  (*Gc.minor ();*)
  let f tid () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size/4 - 1 do
        let x = Atomic.fetch_and_add (Array.get arr (4*i + tid)) 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn @@ f 0 in
  let domain1 = Domain.spawn @@ f 1 in
  let domain2 = Domain.spawn @@ f 2 in
  let domain3 = Domain.spawn @@ f 3 in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let flat_subsequent_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.make arr_size 0 in
  (*Gc.minor ();*)
  let f tid () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size/4 - 1 do
        let x = Atomic.Array.fetch_and_add arr (4*i + tid) 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn @@ f 0 in
  let domain1 = Domain.spawn @@ f 1 in
  let domain2 = Domain.spawn @@ f 2 in
  let domain3 = Domain.spawn @@ f 3 in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let padd_subsequent_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.make (8 * arr_size) 0 in
  (*Gc.minor ();*)
  let f tid () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size/4 - 1 do
        let x = Atomic.Array.fetch_and_add arr (8 * (4*i + tid)) 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn @@ f 0 in
  let domain1 = Domain.spawn @@ f 1 in
  let domain2 = Domain.spawn @@ f 2 in
  let domain3 = Domain.spawn @@ f 3 in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

(* [x] must be a positive power of 2. *)
let log2 =
  let rec aux acc x =
    if x = 1 then acc else aux (acc+1) (x lsr 1)
  in
  function x -> aux 0 x

let glen_subsequent_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  (* Check that array size is a power of two (not required but makes index
     remapping computations faster). *)
  assert (arr_size land (arr_size-1) = 0);
  assert (arr_size >= 8);
  let arr = Array.make arr_size 0 in
  let logp = log2 arr_size - 3 in
  let p = 1 lsl logp in
  let divshift = logp in
  let modmask = p - 1 in
  (*Gc.minor ();*)
  let f tid () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size/4 - 1 do
        let idx = 4*i + tid in (* logical index *)
        let real_idx = ((idx land modmask) lsl 3) + (i lsr divshift) in
        let x = Atomic.Array.fetch_and_add arr real_idx 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn @@ f 0 in
  let domain1 = Domain.spawn @@ f 1 in
  let domain2 = Domain.spawn @@ f 2 in
  let domain3 = Domain.spawn @@ f 3 in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let flat_random_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.make arr_size 0 in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  let f () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size-1 do
        let idx = Array.get arr_idx i in
        let x = Atomic.Array.fetch_and_add arr idx 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn f in
  let domain1 = Domain.spawn f in
  let domain2 = Domain.spawn f in
  let domain3 = Domain.spawn f in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let boxed_random_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  let f () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size-1 do
        let idx = Array.get arr_idx i in
        let x = Atomic.fetch_and_add (Array.get arr idx) 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn f in
  let domain1 = Domain.spawn f in
  let domain2 = Domain.spawn f in
  let domain3 = Domain.spawn f in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let padd_random_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  let arr = Array.make (8*arr_size) 0 in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  let f () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size-1 do
        let idx = Array.get arr_idx i in
        let x = Atomic.Array.fetch_and_add arr (8*idx) 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn f in
  let domain1 = Domain.spawn f in
  let domain2 = Domain.spawn f in
  let domain3 = Domain.spawn f in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let glen_random_parwrite arr_size =
  assert (arr_size mod 4 = 0);
  (* Check that array size is a power of two (not required but makes index
     remapping computations faster). *)
  assert (arr_size land (arr_size-1) = 0);
  assert (arr_size >= 8);
  let logp = log2 arr_size - 3 in
  let p = 1 lsl logp in
  let divshift = logp in
  let modmask = p - 1 in
  let arr = Array.make arr_size 0 in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  let f () =
    for _ = 0 to iterations-1 do
      for i = 0 to arr_size-1 do
        let idx = Array.get arr_idx i in
        let real_idx = ((idx land modmask) lsl 3) + (i lsr divshift) in
        let x = Atomic.Array.fetch_and_add arr real_idx 42 in
        Sys.opaque_identity x |> ignore
      done
    done
  in
  let domain0 = Domain.spawn f in
  let domain1 = Domain.spawn f in
  let domain2 = Domain.spawn f in
  let domain3 = Domain.spawn f in
  Domain.join domain0;
  Domain.join domain1;
  Domain.join domain2;
  Domain.join domain3

let () =
  Random.self_init ();
  for i = 0 to Array.length array_sizes - 1 do
    let array_size = array_sizes.(i) in
    (*
    run_timed "flat_sequential_read " !array_size flat_sequential_read;
    run_timed "boxed_sequential_read" !array_size boxed_sequential_read;
    run_timed "flat_random_read     " !array_size flat_random_read;
    run_timed "boxed_random_read    " !array_size boxed_random_read;
    *)
    run_timed "flat_subsequent_parwrite" array_size flat_subsequent_parwrite;
    run_timed "boxed_subsequent_parwrite" array_size boxed_subsequent_parwrite;
    run_timed "padd_subsequent_parwrite" array_size padd_subsequent_parwrite;
    run_timed "glen_subsequent_parwrite" array_size glen_subsequent_parwrite;
    run_timed "boxed_random_parwrite" array_size boxed_random_parwrite;
    run_timed "flat_random_parwrite " array_size flat_random_parwrite;
    run_timed "padd_random_parwrite " array_size padd_random_parwrite;
    run_timed "glen_random_parwrite " array_size glen_random_parwrite;
  done;
  let gc_stat = Gc.quick_stat () in
  Printf.printf
    "major collections: %d, minor collections: %d, minor allocated: %f\n\n"
    gc_stat.Gc.major_collections
    gc_stat.Gc.minor_collections
    gc_stat.Gc.minor_words
