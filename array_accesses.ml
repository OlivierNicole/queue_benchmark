let iterations = 10_000

let array_size = ref 1024

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

let atomic_array_sequential arr_size =
  let arr = Array.make arr_size 0 in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let x = Atomic.Array.get arr i in
        Sys.opaque_identity(x) |> ignore
    done
  done

let atomic_arrayt_sequential arr_size =
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let x = Array.get arr i |> Atomic.get in
        Sys.opaque_identity(x) |> ignore
    done
  done;
  ignore (Sys.opaque_identity root)

let atomic_array_random arr_size =
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

let atomic_arrayt_random arr_size =
  let arr = Array.init arr_size (fun _ -> Atomic.make 0) in
  let arr_idx = Array.init arr_size (fun _ -> Random.int arr_size) in
  Gc.minor ();
  for _ = 0 to iterations-1 do
    for i = 0 to arr_size-1 do
      let idx = Array.get arr_idx i in
      let x = Array.get arr idx |> Atomic.get in
        Sys.opaque_identity(x) |> ignore
    done
  done;
  ignore (Sys.opaque_identity root)



let () =
  Random.self_init ();
  for _ = 1 to 9 do
    array_size := !array_size * 2;
    run_timed "atomic_array_sequential " !array_size atomic_array_sequential;
    run_timed "atomic_arrayt_sequential" !array_size atomic_arrayt_sequential;
    run_timed "atomic_array_random     " !array_size atomic_array_random;
    run_timed "atomic_arrayt_random    " !array_size atomic_arrayt_random;
  done
