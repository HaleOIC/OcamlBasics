(* Polytypes *)
(* Like let id x = x 
    val f : bool -> bool = <fun> *)
let f x = if x then x else x;;

(* val g : 'a -> bool -> 'a = <fun> *)
let g x y = if y then x else x;;

(* val h : bool -> 'a -> 'a -> 'a = <fun> *)
let h x y z = if x then y else z;;

(* val i : bool -> 'a -> 'b -> 'a = <fun> *)
let i x y z = if x then y else y;;