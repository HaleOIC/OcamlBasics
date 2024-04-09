(** [date m d] is validity of date month [m] day [d]  *)
let date (m: string) (d: int) = 
  match m with
  | "Jan" | "Mar" | "May" | "July" | "Aug" | "Oct" | "Dec" -> 0 < d && d < 32 
  | "Apr" | "Jun" | "Sept" | "Nov" -> 0 < d && d < 31
  | "Feb" -> 0 < d && d < 29 
  | _ -> false;;

date "Feb" 30;;