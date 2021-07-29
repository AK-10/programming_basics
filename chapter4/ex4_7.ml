(* 鶴の数crane_num, 亀の数turtle_numを受け取ってその総数を返す *)
(* int -> int -> int *)
let crane_turtle_legs_sum crane_num turtle_num = crane_num * 2 + turtle_num * 4;;

let test0 = crane_turtle_legs_sum 0 0 = 0;;
let test1 = crane_turtle_legs_sum 1 1 = 6;;
let test2 = crane_turtle_legs_sum 3 2 = 14;;
let test3 = crane_turtle_legs_sum 100 20 = 280;;
