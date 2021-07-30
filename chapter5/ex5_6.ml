(* hanbetsusiki: float -> float -> float -> float *)
let hanbetsusiki a b c = b ** 2.0 -. 4.0 *. a *.c ;;

(* hanbetsusiki: float -> float -> float -> bool *)
let kyosuukai a b c = hanbetsusiki a b c < 0.;;

let test0 = kyosuukai 0. 0. 0. = false;;
let test1 = kyosuukai 1. 3. 2. = false;;
let test2 = kyosuukai 4. 1. 1. = true;;
