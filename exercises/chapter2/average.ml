(* Define an infix operator +/. to compute the average of two floating-point numbers. *)
let ( +/. ) = fun x y -> (x +. y) /. 2.;;

assert (1.0 +/. 2. = 1.5);;
assert (0. +/. 0. = 0.);;