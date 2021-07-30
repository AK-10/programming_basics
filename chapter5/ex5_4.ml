(* ax^2 + bx +c = 0 のa,b,cを受け取り, 判別式の値を返す *)
(* hanbetsusiki: float -> float -> float -> float *)
let hanbetsusiki a b c = b ** 2.0 -. 4.0 *. a *.c ;;

let test0 = hanbetsusiki 1. 3. 2. = 1.;;
let test0 = hanbetsusiki 1. 5. 4. = 9.;;
let test0 = hanbetsusiki 2. (-4.) 2. = 0.;;
