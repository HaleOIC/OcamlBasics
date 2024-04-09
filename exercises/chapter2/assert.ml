assert true;;
(* - : unit = () *)

assert false;;
(* Exception: Assert_failure ("assert.ml", 4, 0). *)

assert ( 2110 != 3110 );;
(* Write an expression that asserts 2110 is not (structurally) equal to 3110 *)
