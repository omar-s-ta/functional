module Tlist = Tlist

let do_it = "Just Do It"

let curry f a b = f (a, b)

let uncurry f (a, b) = f a b
