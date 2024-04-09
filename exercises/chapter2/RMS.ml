(********************************************************************
 * exercise: RMS
 ********************************************************************)
 let close_enough (x: float)  (y: float) = 
 Float.abs (x -. y) < 1e-5;;
 let rms x y = sqrt ((x *. x +. y *. y) /. 2.);;


 let _ = assert (close_enough (rms 2. 2.) 2.)
 let _ = assert (close_enough (rms 7. 42.) 30.10813)