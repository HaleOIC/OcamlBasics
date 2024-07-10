open OUnit2
open Ps1

(* EXERCISE 1 *)
let test_is_mon_inc _ =
  assert_bool "is_mon_inc_test1" ((is_mon_inc [1;2;3;6;9]) = true);
  assert_bool "is_mon_inc_test2" ((is_mon_inc [1;3;5;7;5;9]) = false);
  assert_bool "is_mon_inc_test3" ((is_mon_inc [1;1;2;3;4;4]) = true);
  assert_bool "is_mon_inc_test4" ((is_mon_inc [1]) = true);
  assert_bool "is_mon_inc_test5" ((is_mon_inc [42;5]) = false);
  assert_bool "is_mon_inc_test6" ((is_mon_inc [5;42]) = true);
  assert_bool "is_mon_inc_test7" ((is_mon_inc [13;13]) = true);
  assert_bool "is_mon_inc_test8" ((is_mon_inc [1;10;3]) = false);
  assert_bool "is_mon_inc_test9" ((is_mon_inc [5;4;3;2;1]) = false);
  assert_bool "is_mon_inc_test10" ((is_mon_inc []) = true);
  assert_bool "is_mon_inc_test11" ((is_mon_inc [10000]) = true)

(* EXERCISE 2 *)
let test_is_unimodal _ =
  assert_bool "is_unimodal_test1" ((is_unimodal [1;2;3;6;9;5;4]) = true);
  assert_bool "is_unimodal_test2" ((is_unimodal [1;3;5;7;5;6]) = false);
  assert_bool "is_unimodal_test3" ((is_unimodal [1;1;2;3;4;4;3;2;2;-1]) = true);
  assert_bool "is_unimodal_test4" ((is_unimodal []) = true);
  assert_bool "is_unimodal_test5" ((is_unimodal [1;1;1]) = true);
  assert_bool "is_unimodal_test6" ((is_unimodal [1;2]) = true);
  assert_bool "is_unimodal_test7" ((is_unimodal [2;1]) = true);
  assert_bool "is_unimodal_test9" ((is_unimodal [0]) = true);
  assert_bool "is_unimodal_test10" ((is_unimodal [0;0;0]) = true);
  assert_bool "is_unimodal_test11" ((is_unimodal [-4;-3;-4]) = true);
  assert_bool "is_unimodal_test12" ((is_unimodal [-3;-2;-1;0]) = true);
  assert_bool "is_unimodal_test13" ((is_unimodal [-2;0;2;-3]) = true);
  assert_bool "is_unimodal_test14" ((is_unimodal [0;1;2;1;0;1]) = false)

(* EXERCISE 3 *)
let test_powerset _ =
  assert_bool "powerset_test1" ((powerset [1;2;3]) = [[1;2;3];[1;2];[1;3];[1];[2;3];[2];[3];[]]);
  assert_bool "powerset_test2" ((powerset []) = [[]]);
  assert_bool "powerset_test3" ((powerset [1]) = [[1];[]]);
  assert_bool "powerset_test4" ((powerset [4;1]) = [[4;1];[4];[1];[]]);
  assert_bool "powerset_test5" ((powerset [10;9;8;7]) = [[10;9;8;7];[10;9;8];[10;9;7];[10;9];[10;8;7];[10;8];[10;7];[10];[9;8;7];[9;8];[9;7];[9];[8;7];[8];[7];[]]);
  assert_bool "powerset_test6" ((powerset [1;1;1]) = [[1;1;1];[1;1];[1;1];[1];[1;1];[1];[1];[]]);
  assert_bool "powerset_test7" ((powerset [4;2;6;2;6;4]) = [[4;2;6;2;6;4];[4;2;6;2;6];[4;2;6;2;4];[4;2;6;2];[4;2;6;6;4];[4;2;6;6];[4;2;6;4];[4;2;6];[4;2;2;6;4];[4;2;2;6];[4;2;2;4];[4;2;2];[4;2;6;4];[4;2;6];[4;2;4];[4;2];[4;6;2;6;4];[4;6;2;6];[4;6;2;4];[4;6;2];[4;6;6;4];[4;6;6];[4;6;4];[4;6];[4;2;6;4];[4;2;6];[4;2;4];[4;2];[4;6;4];[4;6];[4;4];[4];[2;6;2;6;4];[2;6;2;6];[2;6;2;4];[2;6;2];[2;6;6;4];[2;6;6];[2;6;4];[2;6];[2;2;6;4];[2;2;6];[2;2;4];[2;2];[2;6;4];[2;6];[2;4];[2];[6;2;6;4];[6;2;6];[6;2;4];[6;2];[6;6;4];[6;6];[6;4];[6];[2;6;4];[2;6];[2;4];[2];[6;4];[6];[4];[]])

(* EXERCISE 4 *)
let test_rev_int _ =
  assert_bool "rev_int_test1" ((rev_int 1234) = 4321);
  assert_bool "rev_int_test2" ((rev_int 4) = 4);
  assert_bool "rev_int_test3" ((rev_int (-1234)) = (-4321));
  assert_bool "rev_int_test4" ((rev_int (-10)) = (-1));
  assert_bool "rev_int_test5" ((rev_int 111111111) = 111111111);
  assert_bool "rev_int_test6" ((rev_int 1010101) = 1010101);
  assert_bool "rev_int_test7" ((rev_int 1) = 1);
  assert_bool "rev_int_test8" ((rev_int 1000) = 1);
  assert_bool "rev_int_test9" ((rev_int (-1)) = (-1));
  assert_bool "rev_int_test10" ((rev_int (-1000)) = (-1))

(* EXERCISE 5 *)
let test_unflatten _ =
  assert_bool "unflatten_test1" ((unflatten (-1) []) = None);
  assert_bool "unflatten_test2" ((unflatten 0 [1;2;3;4;5;6]) = None);
  assert_bool "unflatten_test3" ((unflatten 2 [1;2;3;4;5;6]) = Some [[1;2];[3;4];[5;6]]);
  assert_bool "unflatten_test4" ((unflatten 3 [1;2;3;4;5;6;7;8]) = Some [[1;2;3];[4;5;6];[7;8]]);
  assert_bool "unflatten_test5" ((unflatten 6 [1;2;3;4;5;6]) = Some [[1;2;3;4;5;6]]);
  assert_bool "unflatten_test6" ((unflatten 7 [1;2;3;4;5;6]) = Some [[1;2;3;4;5;6]]);
  assert_bool "unflatten_test7" ((unflatten 1000 ['c';'s';'3';'1';'1';'0']) = Some [['c';'s';'3';'1';'1';'0']]);
  assert_bool "unflatten_test8" ((unflatten 2 ['c';'s';'3';'1';'1';'0']) = Some [['c';'s'];['3';'1'];['1';'0']]);
  assert_bool "unflatten_test9" ((unflatten 3 ['c';'s';'3';'1';'1';'0']) = Some [['c';'s';'3'];['1';'1';'0']]);
  assert_bool "unflatten_test10" ((unflatten 4 ['c';'s';'3';'1';'1';'0']) = Some [['c';'s';'3';'1'];['1';'0']]);
  assert_bool "unflatten_test11" ((unflatten 5 [true;false;true;true;false;false;false;true]) = Some [[true;false;true;true;false];[false;false;true]]);
  assert_bool "unflatten_test12" ((unflatten 1 ["panda";"eats";"shoots";"and";"leaves"]) = Some [["panda"];["eats"];["shoots"];["and"];["leaves"]])

(* EXERCISE 6 *)
let test_int_of_roman _ =
  assert_bool "int_of_roman_test1" ((int_of_roman [I;I;I]) = 3);
  assert_bool "int_of_roman_test2" ((int_of_roman [X;L;I;I]) = 42);
  assert_bool "int_of_roman_test3" ((int_of_roman [M;C;M;X;C;I;X]) = 1999);
  assert_bool "int_of_roman_test4" ((int_of_roman []) = 0);
  assert_bool "int_of_roman_test5" ((int_of_roman [V;I;I]) = 7);
  assert_bool "int_of_roman_test6" ((int_of_roman [M]) = 1000);
  assert_bool "int_of_roman_test7" ((int_of_roman [X;I;V]) = 14);
  assert_bool "int_of_roman_test8" ((int_of_roman [C;C;C;X;L;I;X]) = 349)

let suite =
  "Test Ps1" >::: [
    "test_is_mon_inc" >:: test_is_mon_inc;
    "test_is_unimodal" >:: test_is_unimodal;
    "test_powerset" >:: test_powerset;
    "test_rev_int" >:: test_rev_int;
    "test_unflatten" >:: test_unflatten;
    "test_int_of_roman" >:: test_int_of_roman;
  ]

let () =
  run_test_tt_main suite
