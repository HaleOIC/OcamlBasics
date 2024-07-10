open Core;;

(* let <variable> = <expr>, bind a specific value to a given variable *)
let x = 3
let y = 4
let z = x + y;;
Printf.printf "basic binding: %d\n" z

(* let <variable> = <expr1> in <expr2> *)
let languages = "OCaml,Perl,C++,C";;

let dashed_languages =
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list;;
Printf.printf "dashed language: %s\n" dashed_languages;;

(* nested let *)
let area_of_ring inner_radius outer_radius =
  let pi = Float.pi in 
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius;;
Printf.printf "area of ring: %f\n" (area_of_ring 1.0 3.0)

let upcase_first_line line = 
  match String.split line ~on: ',' with 
  | [] -> assert false
  | first :: rest -> String.concat ~sep: "," (String.uppercase first :: rest);;
Printf.printf "upcase first line outcome: %s \n" (upcase_first_line "test, for first line, this only a test");;

(* Anonymous function *)
let add = fun x y -> x + y;;
Printf.printf "anonymous function: %d\n" (add 3 4)

let transforms = [ String.uppercase; String.lowercase; String.capitalize ]
let arrays = List.map transforms ~f:(fun f -> f "hello world");;
Printf.printf "anonymous function in list: %s\n" (String.concat ~sep: ", " arrays)

(* partial application and curry*)
let abs_diff x y = abs (x - y)
let abs_diff_3 = abs_diff 3;;
Printf.printf "partial application: %d\n" (abs_diff_3 0)

(* recursive function *)
let rec find_first_repeat list = 
  match list with
    (* [] matches the empty list, and [_] matches any single element list *)
    | [] | [_] -> None 
  | x :: y :: tl -> 
    if x = y then Some x
    else find_first_repeat (y :: tl);;
Printf.printf "find first repeat outcome: %d" 
  (match find_first_repeat [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100] with
    | Some x -> x
    | None -> 0)

(* Prefix and Infix Operators *)
let ( +/ ) x y = x + y;;
Printf.printf "infix operator: %d\n" (3 +/ 4)
let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2);;
let () = 
  let (x, y) = (+!) (1, 2) (3, 4) in 
  Printf.printf "prefix operator: %d, %d\n" x y;;

(* Operator Precedence *)
(* mod, lsl(logical shift left) *)
let (|>) x f = f x;; (* Reverse application operator *)
let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin";;
String.split ~on:':' path
|> List.dedup_and_sort ~compare:String.compare
|> List.iter ~f:print_endline;;

(* f( g (h x)) <-> f @@ g @@ h x right associative *)

let some_or_default default = function 
  | Some x -> x
  | None -> default;;
Printf.printf "operator precedence: %d\n" (some_or_default 0 (Some 3))

(* Labeled Arguments *)
let ratio ~num ~denom = Float.of_int num /. Float.of_int denom;;
Printf.printf "labeled arguments: %f\n" (ratio ~denom:4 ~num:3);;
Printf.printf "labeled arguments: %f\n" (let num = 3 in
let denom = 4 in
ratio ~num ~denom);;

(* Higher-Order functions and labels *)
let apply_to_tuple f (first,second) = f ~first ~second;;
Printf.printf "higher-order functions and labels: %d\n" (apply_to_tuple (fun ~first ~second -> first + second) (3,4))

(* Optional Arguments *)
let concat ?sep x y =
  let sep = match sep with None -> "" | Some s -> s in
  x ^ sep ^ y;;
Printf.printf "optional arguments: %s\n" (concat "hello" "world");;
(* ^ operator for pairwise string concatenation. *)
let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b;;
Printf.printf "optional arguments: %s\n" (uppercase_concat "hello" "world");;

(* Inference of labeled and optional arguments *)
let numeric_deriv ~delta ~x ~y ~(f: x:float -> y:float -> float) =
  let x' = x +. delta in
  let y' = y +. delta in
  let base = f ~x ~y in
  let dx = (f ~x:x' ~y -. base) /. delta in
  let dy = (f ~x ~y:y' -. base) /. delta in
  (dx, dy);;

let delta = 0.1 in
let x = 1.0 in
let y = 2.0 in
let f ~x ~y = x *. y in
let (dx, dy) = numeric_deriv ~delta ~x ~y ~f in
Printf.printf "inference of labeled and optional arguments: %f, %f\n" dx dy;;


(* an optional argument is erased as soon as the first positional (i.e., neither labeled nor optional) 
argument defined after the optional argument is passed in *)

let concat x ?(sep="") y = x ^ sep ^ y;;
(* val concat : string -> ?sep:string -> string -> string = <fun> *)
let prepend_pound = concat "# ";;
(* val prepend_pound : ?sep:string -> string -> string = <fun> *)
Printf.printf "optional argument is erased: %s\n" (prepend_pound "hello");;

