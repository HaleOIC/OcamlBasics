(** [fib n] is the n-th number in the Fibonacci sequence *)
let rec fib n = 
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n - 1) + fib (n - 2);;

fib 10;;
(* fib 50;; it will take too much time *)


(* helper function tail recursion(tr)*)
let rec h n pp p = 
  if n = 1 then p
  else h (n - 1) p (pp + p);;

let fib_fast n = 
  h n 0 1;;

fib_fast 50;;

