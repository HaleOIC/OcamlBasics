open Core

(* Basic arithmetic operations *)
let x = 3 + 4
let y = x + x
let x_plus_y = x + y 
let () = Printf.printf "x_plus_y: %d\n" x_plus_y  (* Print x_plus_y *)

(* Simple functions *)
let square x = x * x 
let () = Printf.printf "square 3: %d\n" (square 3)  (* Print example output of square function *)

let ratio x y = Float.of_int x /. Float.of_int y 
let () = Printf.printf "ratio 3 4: %F\n" (ratio 3 4)  (* Print example output of ratio function *)

(* Higher-order function example *)
let sum_if_true test first second = 
  (if test first then first else 0) + (if test second then second else 0)
let () = Printf.printf "sum_if_true (fun x -> x > 2) 3 1: %d\n" (sum_if_true (fun x -> x > 2) 3 1)  (* Print example output of sum_if_true function *)

let first_if_true test x y =
  if test x then x else y
let () = Printf.printf "first_if_true (fun x -> x > 2) 3 1: %d\n" (first_if_true (fun x -> x > 2) 3 1)  (* Print example output of first_if_true function *)

(* Tuple example *)
let tuple = (3, "three")
let () = Printf.printf "tuple: (%d, %s)\n" (fst tuple) (snd tuple)  (* Print tuple *)

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
let () = Printf.printf "distance (3., 4.) (0., 0.): %F\n" (distance (3., 4.) (0., 0.))  (* Print example output of distance function *)

(* List operations *)
let languages = ["OCaml"; "Perl"; "C"]
let lengths = List.map ~f:String.length languages
let () = Printf.printf "lengths of languages: %s\n" (String.concat ~sep:", " (List.map ~f:Int.to_string lengths))  (* Print lengths of languages *)

let string_name = "French" :: "Spanish" :: "English" :: []
let () = Printf.printf "string_name: %s\n" (String.concat ~sep:", " string_name)  (* Print string_name *)

(* Pattern matching and recursion *)
let my_favorite_language languages =
  match languages with
  | first :: _ -> first
  | [] -> "OCaml"
let () = Printf.printf "my_favorite_language: %s\n" (my_favorite_language languages)  (* Print my_favorite_language *)

let rec sum list =
  match list with
  | [] -> 0
  | hd :: tl -> hd + sum tl
let () = Printf.printf "sum of [1; 2; 3]: %d\n" (sum [1; 2; 3])  (* Print sum of a list *)

let rec destutter list = 
  match list with 
  | [] -> []
  | [hd] -> [hd]
  | hd1 :: hd2 :: tl -> 
    if hd1 = hd2 then destutter (hd2 :: tl)
    else hd1 :: destutter (hd2 :: tl)
let () = Printf.printf "destutter of [1; 1; 2; 3]: %s\n" (String.concat ~sep:", " (List.map ~f:Int.to_string (destutter [1; 1; 2; 3])))  (* Print destuttered list *)

(* Option example *)
let divide x y = if y = 0 then None else Some (x / y)
let () = Printf.printf "divide 4 2: %s\n" (match divide 4 2 with None -> "None" | Some v -> Int.to_string v)  (* Print example output of divide function *)

(* Record and Variants *)
type point2d = { x: float; y: float }
let p = { x = 3.; y = 4. }
let () = Printf.printf "point2d p: (%F, %F)\n" p.x p.y  (* Print point2d record *)

let magnitude { x = x_pos; y = y_pos } = 
  Float.sqrt (x_pos **. 2. +. y_pos **. 2.)
let () = Printf.printf "magnitude of p: %F\n" (magnitude p)  (* Print example output of magnitude function *)

type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }
type scene_element = 
  | Circle of circle_desc
  | Rect of rect_desc
  | Segment of segment_desc

let is_inside_scene_element point scene_element = 
  match scene_element with
  | Circle { center; radius } ->
    let dist = magnitude { x = center.x -. point.x; y = center.y -. point.y } in
    Float.(dist < radius)
  | Rect { lower_left; width; height } ->
    Float.(point.x > lower_left.x && point.x < lower_left.x +. width)
    && Float.(point.y > lower_left.y && point.y < lower_left.y +. height)
  | Segment _ -> false
let () = Printf.printf "is_inside_scene_element: %B\n" (is_inside_scene_element p (Circle { center = p; radius = 5. }))  Print example output of is_inside_scene_element

(* Array operations *)
let numbers = [|1; 2; 3; 4|]
let () =
  numbers.(2) <- 4;
  Array.iter numbers ~f:(Printf.printf "%d ");
  Printf.printf "\n"

(* Mutable record *)
type running_sum = {
  mutable sum: float;
  mutable sum_sq: float;
  mutable samples: int;
}
let mean rsum = rsum.sum /. float rsum.samples
let stdev rsum = 
  sqrt (rsum.sum_sq /. float rsum.samples
        -. (rsum.sum /. float rsum.samples) ** 2.)
let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
let update rsum x = 
  rsum.samples <- rsum.samples + 1;
  rsum.sum <- rsum.sum +. x;
  rsum.sum_sq <- rsum.sum_sq +. x *. x
let () =
  let rsum = create () in
  update rsum 1.0;
  update rsum 2.0;
  update rsum 3.0;
  Printf.printf "mean: %F, stdev: %F\n" (mean rsum) (stdev rsum)  (* Print mean and stdev of running_sum *)

(* Ref example *)
let x_ref = ref 0
let () =
  Printf.printf "Initial value: %d\n" !x_ref;
  x_ref := !x_ref + 1;
  Printf.printf "Updated value: %d\n" !x_ref

(* Custom ref type and operations *)
type 'a ref = { mutable contents: 'a }
let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x
let sum list = 
  let sum = ref 0 in 
  List.iter ~f:(fun x -> sum := !sum + x) list;
  !sum
let () = Printf.printf "sum of [1; 2; 3]: %d\n" (sum [1; 2; 3])  (* Print sum of a list using custom ref *)

(* Function to permute an array *)
let permute array = 
  let length = Array.length array in
  for i = 0 to length - 2 do
    let j = i + Random.int (length - i) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done

(* Initialize an array and permute it *)
let () =
  let ar = Array.init 20 ~f:(fun i -> i) in
  permute ar;
  Array.iter ar ~f:(Printf.printf "%d ");
  Printf.printf "\n"

(* Function to find the first negative entry in an array *)
let find_first_negative_entry array = 
  let pos = ref 0 in
  while 
    !pos < Array.length array && array.(!pos) >= 0
  do
    pos := !pos + 1
  done;
  if !pos = Array.length array then None else Some !pos
let () = Printf.printf "find_first_negative_entry [|1; 2; -1; 4|]: %s\n" 
  (match find_first_negative_entry [|1; 2; -1; 4|] with None -> "None" | Some v -> Int.to_string v)  (* Print example output of find_first_negative_entry *)

(* Complete program to read and accumulate input *)
let rec read_and_accumulate accum =
  match In_channel.input_line In_channel.stdin with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () = 
  printf "Total: %F\n" (read_and_accumulate 0.)
