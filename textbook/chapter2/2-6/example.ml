print_endline "Camels are bae";;

(* Unit is `()` like `void` in Java, means nothing to return *)
print_endline;;


(* The let _ = e syntax above is a way of evaluating e 
   but not binding its value to any name. *)
let _ = print_endline "Camels" in
let _ = print_endline "are" in
print_endline "bae";;


(* The expression e1; e2 first evaluates e1, 
   which should evaluate to (), 
   then discards that value, and evaluates e2 *)
print_endline "Camels";
print_endline "are";
print_endline "bae";;


(* If e1 does not have type unit, then e1; e2 will give a warning，
   `ignore` can help it out *)
(ignore 3); 5;;


(** [print_stat name num] prints [name: num]. *)
let print_stat name num =
  print_string name;
  print_string ": ";
  print_float num;
  print_newline ();;
print_stat "mean" 84.39;;

(* Advanced version to print *)
let print_stat name num =
  Printf.printf "%s: %F\n%!" name num;;
print_stat "mean" 84.39;;
(* %F forces fload, %i forces int, %s forces string *)


(* sprintf, which collects the output in string instead of printing it *)
let string_of_stat name num =
  Printf.sprintf "%s: %F" name num;;
string_of_stat "mean" 84.39;;
