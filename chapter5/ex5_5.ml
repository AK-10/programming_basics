(* ax^2 + bx +c = 0 のa,b,cを受け取り, 判別式の値を返す *)
(* hanbetsusiki: float -> float -> float -> float *)
let hanbetsusiki a b c = b ** 2.0 -. 4.0 *. a *.c ;;

(* ax^2 + bx +c = 0 のa,b,cを受け取り, 解の個数を返す *)
(* hanbetsusiki: float -> float -> float -> int *)
let kai_no_kosuu a b c = if hanbetsusiki a b c > 0.
                         then 2
                         else if hanbetsusiki a b c = 0.
                         then 1
                         else 0;;

let test0 = kai_no_kosuu 0. 0. 0. = 1;;
let test1 = kai_no_kosuu 1. 3. 2. = 2;;
let test0 = kai_no_kosuu 4. 1. 1. = 0;;
