let rec sum =
  function [] -> 0 | x :: xs -> x + sum xs

let () = print_endline "Chapter 3"
