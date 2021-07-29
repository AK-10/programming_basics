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
