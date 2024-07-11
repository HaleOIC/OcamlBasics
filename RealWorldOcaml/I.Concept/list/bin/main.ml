(* open Base;; *)
open Core;;

(* :: operator is right-associative *)
let l = 1 :: 2 :: 3 :: []
let m = 0 :: l
let rec sum l = 
  match l with 
  | [] -> 0
  | hd :: tl -> hd + sum tl;;
Printf.printf "%d\n" (sum m);;

(* using patterns to extract data *)
let rec drop_value l to_drop = 
  match l with
  | [] -> []
  | hd :: tl ->
    let new_tl = drop_value tl to_drop in
    if hd = to_drop then new_tl else hd :: new_tl;;

let () =
  List.map (drop_value [1; 2; 3] 2) ~f:string_of_int
  |> String.concat ~sep:"; "
  |> Printf.printf "[%s]\n";;

(* but particular dropped value is valid *)
let rec drop_zero l = 
  match l with 
  | [] -> []
  | 0 :: tl -> drop_zero tl
  | hd :: tl -> hd :: drop_zero tl;;
let () = 
  drop_zero [1; 0; 2; 0; 3] 
  |> List.map ~f:string_of_int
  |> String.concat ~sep:"; "
  |> Printf.printf "[%s]\n";;

(* if statement run less time than match statement *)

(* List operator *)
List.map ~f:String.length ["a"; "bc"; "def"];; (* [1; 2; 3] *)
List.map2_exn ~f:Int.max [1; 2; 3] [3; 2; 1];; (* [3; 2; 3] *)

(* List.fold *)
let sum = List.fold ~init:0 ~f:(+) [1; 2; 3];;
Printf.printf "fold operator outcome: %d\n" sum;;
List.fold ~init:[] ~f:(fun acc hd -> hd :: acc) [1;2;3;4]
|> List.map ~f:string_of_int
|> String.concat ~sep:"; "
|> Printf.printf "fold version of reverse: [%s]\n";;

(* complete functions using list operator *)
let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
        List.map2_exn ~f:Int.max acc (lengths row));;

let render_separator widths =
  let pieces = List.map widths
      ~f:(fun w -> String.make w '-')
  in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|";;

let pad s length =
  s ^ String.make (length - String.length s) ' ';;

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "| " ^ String.concat ~sep:" | " padded ^ " |";;

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths
      :: render_separator widths
      :: List.map rows ~f:(fun row -> render_row row widths)
    );;

Stdio.print_endline
  (render_table
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
]);;

(* Other List operator *)
List.reduce ~f:(+) [1;2;3;4;5]
|> Option.value ~default:0
|> Printf.printf "reduce operator outcome: %d\n";;

(* List.filter or List.filter_map *)
let extensions filenames =
  List.filter_map filenames ~f:(fun fname ->
      match String.rsplit2 ~on:'.' fname with
      | None  | Some ("",_) -> None
      | Some (_,ext) ->
        Some ext)
  |> List.dedup_and_sort ~compare:String.compare;;

extensions ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"]
|> List.iter ~f:Stdio.print_endline;;

(* List.partition_tf *)
let is_ocaml_source s = 
  match String.rsplit2 s ~on:'.' with 
  | Some(_, ("ml"|"mli")) -> true
  | _ -> false

let (ml_files, other_files) = 
  List.partition_tf ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"]  ~f:is_ocaml_source;;
ml_files |> String.concat ~sep: ", " |> Printf.printf "ml files: [%s]\n";;
other_files |> String.concat ~sep: ", " |> Printf.printf "other files: [%s]\n";;


(* Tail Recursion
let rec length = function 
  | [] -> 0
  | _ :: tl -> 1 + length tl;;  will cause stack overflow *)
let make_list n = List.init n ~f:(fun x -> x);; 
let rec length_plus_n l n =
  match l with 
  | [] -> n
  | _ :: tl -> length_plus_n tl (n + 1);;
let length l = length_plus_n l 0;;
let () = 
  Printf.printf "list length: %d\n" (length @@ make_list 100);;
  Printf.printf "list length: %d\n" (length @@ make_list 10_000_000);;

(* using an as pattern, which allows us to declare a name for the thing matched by a pattern or subpattern *)
open Base.Poly;;
let rec remove_sequential_duplicates list = 
  match list with 
  | [] | [_] as l -> l
  | first :: (second :: tl) when first = second -> 
    remove_sequential_duplicates (second :: tl)
  | first :: tl -> first :: remove_sequential_duplicates tl;;

let result = remove_sequential_duplicates [1; 2; 2; 3; 4; 5; 5]
  |> List.map ~f:string_of_int 
  |> String.concat ~sep:", ";;

Printf.printf "[%s]\n" result

(* Polynomial *)
let rec count_some list = 
  match list with 
  | [] -> 0
  | x :: tl when Option.is_none x -> count_some tl
  | _ :: tl -> 1 + count_some tl;;
Printf.printf "count some outcome: %d\n" (count_some [None; Some 1]);;
(* Another more sophisticated version *)
let count_some l = List.count ~f:Option.is_some l;;
Printf.printf "count some outcome: %d\n" (count_some [None; Some 1]);;
