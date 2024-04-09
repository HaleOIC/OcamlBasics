(* double fun *)
let double = fun x -> x * 2;;

double 7;;

(** more fun *)
(* computes the cube of a floating-point number. *)
let cube = fun (x: float) -> x ** 3.;;
cube 1.0;;
cube 9.0;;
cube (-1.9);;

(* computes the sign (1, 0, or -1) of an integer *)
let sign = fun x -> 
  if x < 0 then -1 
  else if x = 0 then 0 
  else 1;;

sign (-10);;
sign 10;;
sign 0;;

(* computes the area of a circle given its radius *)
let area_of_circle = fun r: float -> 
  r *. r *. Float.pi
let close_enough (x: float)  (y: float) = 
  Float.abs (x -. y) < 1e-5;;

let _ = assert (close_enough (area_of_circle 1.0) Float.pi)
let _ = assert (close_enough (area_of_circle (Float.sqrt (1. /. Float.pi))) 1.)