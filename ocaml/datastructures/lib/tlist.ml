type 'a tlist =
  | Nil
  | Cons of 'a * 'a tlist

(** [init lst] is a [tlst] initiated from a standard [lst]. *)
let rec init lst = match lst with
  | [] -> Nil
  | h :: t -> Cons (h, init t)

(* 'function' is a syntactic sugar for a function with one argument that is being pattern matched *)
(** [of_list lst] same as [init lst] but with 'function' syntactic sugar. *)
let rec of_list = function
  | [] -> Nil
  | h :: t -> Cons (h, of_list t)

let apply = init

(** [tail tlst] is a [tlst] without the first element. 
    The tail of an empty [tlst] crashes. *)
let tail = function
  | Nil -> failwith "empty list"
  | Cons (_, t) -> t

(** [set_head hd tlst] is a [tlst] with the first element set to [hd].
    Setting the head of an empyt [tlst] crashes. *)
let set_head hd tlst = match tlst with
  | Nil -> failwith "empty list"
  | Cons (_, t) -> Cons (hd, t)
