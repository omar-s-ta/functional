type 'a tlist =
  | Nil
  | Cons of 'a * 'a tlist

let rec init lst = match lst with
  | [] -> Nil
  | h :: t -> Cons (h, init t)

(* 'function' is a syntactic sugar for a function with one argument that is being pattern matched *)
let rec of_list = function
  | [] -> Nil
  | h :: t -> Cons (h, of_list t)

let apply = init

let tail = function
  | Nil -> failwith "empty list"
  | Cons (_, t) -> t

let set_head hd tlst = match tlst with
  | Nil -> failwith "empty list"
  | Cons (_, t) -> Cons (hd, t)
