let date_fun m d =
  if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov" then d > 0 && d <= 30
  else if m = "Feb" then d > 0 && d <= 28
  else d > 0 && d <= 31

let rec fib n =
  if n > 2 then fib (n - 1) + fib (n - 2) else 1

let fib_fast n =
  let rec do_it n f s =
    if n = 1 then s
    else do_it (n - 1) s (f + s)
  in do_it n 0 1

(** for creating 'infix' function it is recommended to leave space before each parenthesis *)
let ( +/. ) a b = (a +. b) /. 2.

let () = print_endline "Chapter 2: Functions"
