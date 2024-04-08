let () = print_endline "Hello, World!";;
65 / 60;;
65 mod 60;;

(* basic type: flaot and int *)
3.;;
3;;
3.14 *. 2.;;

(* convert int and float *)
3.14 *. (float_of_int 2);;
6 * (int_of_float 6.);;

(* bool: true and false, && || has short-circuit conjunction *)

(* Single quotes, such as 'a', 'b' to represent char
    conver int and char *)
5 * int_of_char 'a';;


(* Double quotes for string, ^ for concatenation operator *)
"abc" ^ "def";;

(* there are built-in functions: string_of_int, string_of_float, string_of_bool *)
(* String.make can have the power of string_of_char *)
String.make 1 'z';;

let str0 = "abc" ^ "def";;
print_endline str0;;


(* No char_of_string but we can use index(0-based) *)
let c = "abc".[1];;

(* Operators = and <> examine structural equality 
   whereas == and != examine physical equality. *)

(* Assert/ assert e /, if the result is true, nothing more happens
    otherwise, an exception is raised *)

(* if expression and type checking *)
let ifvalue = 
  if 2 > 3 then "hahaha"
  else if 7 > 9 then "rediculous"
  else "correct";;

(* let can be expression but also definition(binding) *)
(let x = 1 in x + 1) + 12;;

(* nested version *)
let a = "big" in
  let  b = "red" in 
    let c = a ^ b in 
      print_endline c;;


(* Scope *)
let x = 42 in
  (* y is not meaningful here *)
  x + (let y = "3110" in
          (* y is meaningful here *)
          int_of_string y);;


(* bind will affect until it encounters with a new variable *)
let x = 5 in
  ((let x = 6 in x) + x);;


(* (e: t) means e is type of t *)
(3.: float) +. 0.5;;