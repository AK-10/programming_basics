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

(* 鶴の数crane_num, 亀の数turtle_numを受け取ってその総数を返す *)
(* int -> int -> int *)
let crane_turtle_legs_sum crane_num turtle_num = crane_num * 2 + turtle_num * 4;;

let test0 = crane_turtle_legs_sum 0 0 = 0;;
let test1 = crane_turtle_legs_sum 1 1 = 6;;
let test2 = crane_turtle_legs_sum 3 2 = 14;;
let test3 = crane_turtle_legs_sum 100 20 = 280;;

(* 鶴と亀の数の合計numと鶴と亀の足の数の合計legs_numを受け取り、鶴の数を返す *)
(*
以下の連立方程式より関数が導ける
t: 鶴の数, k: 亀の数
t + k = num
2t + 4k = legs_num

-> t = (4num - log_num) / 2
*)

(* int -> int -> int *)
let tsurukame num legs_num = (num * 4 - legs_num) / 2;;

let test1 = tsurukame 0 0 = 0;;
let test2 = tsurukame 10 30 = 5;;
let test2 = tsurukame 4 16 = 0;;
let test3 = tsurukame 12 34 = 7;;
