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
let set_head hd = function
  | Nil -> failwith "empty list"
  | Cons (_, t) -> Cons (hd, t)

(** [drop n tlst] is a [tlst] without the first n elements. *)
let rec drop n tlst =
  if n <= 0 then tlst
  else match tlst with
    | Nil -> Nil
    | Cons (_, t) -> drop (n - 1) t

(** [drop_while p tlst] is a [tlst] without the first elements that satisfy the predicate *)
let rec drop_while p tlst = match tlst with
  | Cons (h, t) when p h -> drop_while p t
  | _ -> tlst

(** [fold_left f init [b1; b2; ...; bn]] is [f (... (f (f init b1) b2) ...) bn] *)
let rec fold_left f init = function
  | Nil -> init
  | Cons (h, t) -> fold_left f (f init h) t
