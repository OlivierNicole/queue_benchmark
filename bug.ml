let roughly_l1_byte_size = 96_000
let roughly_l1_word_size = roughly_l1_byte_size / 8
let nreads = 1_000_000

let access_flat_array array : unit =
  let rand = Random.State.make_self_init () in
  for _ = 1 to nreads do
    let i = Random.State.int rand roughly_l1_word_size in
    let x = Atomic.Array.fetch_and_add array i 42 in
    ignore (Sys.opaque_identity x)
  done

let access_boxed_array (array : int Atomic.t array) : unit =
  let rand = Random.State.make_self_init () in
  for _ = 1 to nreads do
    let i = Random.State.int rand roughly_l1_word_size in
    let x = Atomic.fetch_and_add array.(i) 42 in
    ignore (Sys.opaque_identity x)
  done

let array_boxed_small : int Atomic.t array =
  Array.make (roughly_l1_word_size / 4) (Atomic.make 42)

  (* Error here... passed typechecker *)
let () = access_flat_array array_boxed_small
