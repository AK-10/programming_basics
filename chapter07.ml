(* 問題7.1 *)

(* 5科目の点数を受け取り、合計を返す *)
(* int -> int -> int -> int -> int -> int *)
let sum a b c d e = a + b + c + d + e;;

(* 5科目の点数を受け取り、合計と平均を返す *)
(* int -> int -> int -> int -> int -> (int, float) *)
let sum_and_avg a b c d e = (sum a b c d e, float (sum a b c d e) /. 5.);;

let test0 = sum_and_avg 0 0 0 0 0 = (0, 0.);;
let test1 = sum_and_avg 80 80 80 80 80 = (400, 80.);;
let test2 = sum_and_avg 75 50 22 53 90 = (290, 58.);;

(* 問題7.2 *)
(* 名前と成績の組を受け取って、「xxさんの評価はyyです」という文字列を返す *)
(* string * string -> string *)
let seiseki pair = match pair with
  (a, b) -> a ^ "さんの評価は" ^ b ^ "です";;

let test0 = seiseki ("wawawa", "A") = "wawawaさんの評価はAです";;
let test1 = seiseki ("山田太郎", "B") = "山田太郎さんの評価はBです";;
let test2 = seiseki ("田中", "C") = "田中さんの評価はCです";;

(* 問題7.3 *)
(* 座標(x, y)を受け取って、x軸について対象な座標を返す *)
(* float * float -> float * float *)
let taisho_x point = match point with
  (x, y) -> (x, -.y);;

let test0 = taisho_x (0., 0.) = (0., 0.);;
let test1 = taisho_x (1., -3.) = (1., 3.);;
let test2 = taisho_x (0., 14.45) = (0., -14.45);;

(* 問題7.4 *)
(* 座標(x1, y1)と(x2, y2)を受け取って、二点間の中点を受け取る *)
(* float * float -> float * float -> float * float *)
let chuten point1 point2 = match (point1, point2) with
  ((x1, y1), (x2, y2)) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.);;

let test0 = chuten (0.0, 0.0) (1.0, 2.0) = (0.5, 1.0);;
let test1 = chuten (2.3, 5.1) (7.6, 1.7) = (4.95, 3.4);;
let test2 = chuten (-3.8, -2.4) (3.4, -1.2) = (-0.2, -1.8);;

