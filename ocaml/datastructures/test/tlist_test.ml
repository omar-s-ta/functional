open Datastructures 
open QCheck

let rec as_tlist = function
  | [] -> Tlist.Nil
  | h :: t -> Tlist.Cons (h, as_tlist t)

let rec as_list = function
  | Tlist.Nil -> []
  | Tlist.Cons (h, t) -> h :: as_list t

let gen_int_list   = Gen.list Gen.int
let gen_float_list = Gen.list Gen.float

let gen_int_tlist   = gen_int_list   |> Gen.map as_tlist
let gen_float_tlist = gen_float_list |> Gen.map as_tlist

let gen_int_tlist_tlist =
  Gen.list gen_int_tlist

let test_failure f =
  try
    ignore @@ f ();
    false
  with
    | _ -> true

let make_test name gen =
  QCheck.Test.make ~name:name (QCheck.make gen)

let test_tail =
  make_test "tail" gen_int_tlist
    (function
    | Tlist.Nil ->
        test_failure (fun () -> Tlist.tail Tlist.Nil)
    | Tlist.Cons (h, t) ->
        let tail = Tlist.tail (Tlist.Cons (h, t)) in
        tail = t)

let test_set_head =
  make_test "set_head" gen_int_tlist
    (function
    | Tlist.Nil ->
        test_failure (fun () -> Tlist.set_head 0 Tlist.Nil)
    | Tlist.Cons (h, t) ->
        let tlst = Tlist.set_head 0 (Tlist.Cons (h, t)) in
        tlst = Tlist.Cons (0, t))

let tests = [test_tail; test_set_head]

let () = QCheck_runner.run_tests_main tests
