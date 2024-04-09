let add x y = x + y;;

(* return 6 *)
let _ = add 5 1

(* int -> int = <fun> *)
let _ = add 5

(* return 6 *)
let _ = (add 5) 1

(* let _ = add (5 1) this will incur error *)
