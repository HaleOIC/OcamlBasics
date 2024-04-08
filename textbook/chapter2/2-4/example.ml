(* recursive function *)
let rec fact n = 
  if n = 0 then 1
  else n * fact (n - 1);;

fact 10;;


(* [pow x y] is [x] to the power of [y] *)
let rec pow x y = 
  if y = 0 then 1
  else x * pow x (y - 1);;


(** [even n] is whether [n] is even.
  Requires: [n >= 0]. *)
let rec even n =
  n = 0 || odd (n - 1)

(** [odd n] is whether [n] is odd.
    Requires: [n >= 0]. *)
and odd n =
  n <> 0 && even (n - 1)

(* fun is the keyword indicating an anonymous function *)
let inc = fun x -> x + 1;;

(* pipeline:
    very useful when use a long function chain to compute *)
let square x = x * x;;
5 |> inc |> square |> inc |> inc |> square;;
square (inc (inc (square (inc 5))));;

(* Polymorphic functions *)
let id x = x;;
let first x y = x;;

(* Labeled and optional arguments *)
let f ~name1:arg1 ~name2:arg2 = arg1 + arg2;;
let f ~name1 ~name2 = name1 + name2;;
let f ~name1:(arg1 : int) ~name2:(arg2 : int) = arg1 + arg2;;
f ~name2:3 ~name1:4;;

let f ?name:(arg1=8) arg2 = arg1 + arg2;;
f ~name:2 7;;
f 7;;

(* Partial function *)
let add x y = x + y;;
let addx x = fun y -> x + y;;
let add5 = addx 5;;
add5 7;;
let add = fun x -> (fun y -> x + y);;

(* Amazing fact:
    Every OCaml function takes exactly one argument. *)


(* Operators as Functions *)
( + ) 3 4;;
let add3 = ( + ) 3;;
add3 2;;
let ( ^^ ) x y = max x y;;
( ^^ ) 10 9;;

(* Tail Recursion *)
(** [count n] is [n], computed by adding 1 to itself [n] times.  That is,
    this function counts up from 1 to [n]. *)
let rec count n =
  if n = 0 then 0 else 1 + count (n - 1);;
count 100_000;;

(* Causing overflow function 
let rec count_forever n = 1 + count_forever n;;
count_forever 1000 *)

(* tail position, which is a technical way of saying 
   "there’s no more computation to be done after it returns" *)

let rec count_aux n acc =
  if n = 0 then acc else count_aux (n - 1) (acc + 1);;

let count_tr n = count_aux n 0


(* The Recipe for Tail Recursion. In a nutshell, here’s how we made a function be tail recursive:
    1. Change the function into a helper function. Add an extra argument: 
    the accumulator, often named acc.
    2. Write a new “main” version of the function that calls the helper. 
    It passes the original base case’s return value as the initial value of the accumulator.
    3. Change the helper function to return the accumulator in the base case.
    4. Change the helper function’s recursive case. It now needs to do the extra work on 
    the accumulator argument, before the recursive call. This is the only step that requires much ingenuity. *)

(* need `#requires Zarith` to load big integer *)
let rec zfact_aux n acc =
  if Z.equal n Z.zero then acc else zfact_aux (Z.pred n) (Z.mul acc n);;

let zfact_tr n = zfact_aux n Z.one;;

zfact_tr (Z.of_int 50);;