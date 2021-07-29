(* crane_numで与えられた鶴の数から足の総数を数える *)
(* int -> int *)
let crane_legs crane_num = 2 * crane_num;;

let test1 = crane_legs 2 = 4;;
let test2 = crane_legs 4 = 8;;
let test3 = crane_legs 100 = 200;;

(* turtle_numで与えられた亀の数から足の総数を数える *)
(* int -> int *)
let turtle_legs turtle_num = 4 * turtle_num;;

let test0 = turtle_legs 0 = 0;;
let test1 = turtle_legs 2 = 8;;
let test2 = turtle_legs 4 = 16;;
let test3 = turtle_legs 100 = 400;;

