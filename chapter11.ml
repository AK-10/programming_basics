(* example 11.2 *)
(* 自然数nの階乗を求める *)
(* fac: int -> int *)
let rec fac n =
  if n = 0
  then 1
  else n * fac (n - 1);;

let test1 = fac 0 = 1;;
let test2 = fac 1 = 1;;
let test3 = fac 2 = 2;;
let test4 = fac 3 = 6;;
let test5 = fac 4 = 24;;
let test4 = fac 10 = 3628800;;

(* ex11.1 *)
(* 0 から 受け取った自然数までの2乗の和を求める *)
(* sum_of_square: int -> int *)
let rec sum_of_square n =
  if n = 0 then
    0
  else
    n * n + sum_of_square (n - 1)

let test1 = sum_of_square 0 = 0;;
let test2 = sum_of_square 1 = 1;;
let test3 = sum_of_square 4 = 30;;

(* ex11.2 *)
(* 数列a_nの第n項を求める *)
(* a: int -> int *)
(* a_n = a_0 = 3 *)
(*     | a_n = 2a_n-1 - 1 (n >= 1) *)

let rec a n =
  if n = 0 then
    3
  else
    2 * (a (n - 1)) - 1

let test1 = a 0 = 3
let test2 = a 1 = 5
let test3 = a 2 = 9
let test4 = a 3 = 17
let test5 = a 4 = 33
