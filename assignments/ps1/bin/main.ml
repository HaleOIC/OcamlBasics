open Ps1;;

let rec is_mon_inc list = 
  match list with
  | [] -> true
  | [_] -> true
  | x::y::tl -> if x <= y then is_mon_inc (y::tl) else false;;