(* ex14.1 *)
(* 値が偶数であるかを調べる(List.filterを利用して実装する) *)
let is_even x = x mod 2 = 0
(* リストlstを受け取って、偶数のみを含むリストを返す *)
(* even: int list -> int list *)
let even = List.filter is_even;;

let test1 = even [] = [];;
let test2 = even [1] = [];;
let test3 = even [2] = [2];;
let test4 = even [1; 2; 3; 4] = [2; 4];;

(* ex14.2 *)
type person_t = {
  name: string;
  height: float;
  weight: float;
  birthday: int * int;
  blood_type: string;
};;

let persons1 = [];;
let persons2 = [
  { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "O" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "eee"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "B" };
  { name = "kkk"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "B" };
  { name = "ooo"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "www"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
];;

(* person_t型のリストを受け取ってA型の血液型の人数を返す
   List.filter, List.length を利用して実装する
 *)
let is_A_type p = match p with
    { blood_type = "A" } -> true
  | _  -> false
(* count_A: person_t list -> int *)
let count_A lst = List.length (List.filter is_A_type lst)

let test1 = count_A persons1 = 0;;
let test2 = count_A persons2 = 3;;

(* ex14.3 *)
let concat_one a b = a ^ b
(* 文字列のリストlstを受け取って、前から順に結合した文字列を返す
   List.fold_rightを利用して実装する
 *)
(* concat: string list -> string *)
let concat lst = List.fold_right concat_one lst "";;

let test1 = concat [] = "";;
let test2 = concat ["a"; "b"; "c"; "d"] = "abcd";;

(* ex14.4 *)
type student_t = {
  name: string; (* 名前 *)
  point: int; (* 点数 *)
  assesment: string; (* 評価 *)
};;

let students1 = [];;
let students2 = [
  {name = "aa"; point = 19; assesment = "C"};
];;
let students3 = [
  {name = "aa"; point = 59; assesment = "B"};
  {name = "bb"; point = 100; assesment = "A"};
  {name = "cc"; point = 1; assesment = "C"};
];;

let add_point a b = match a with
  { point = a_p; } -> a_p + b;;
(* student_t型のlistを受け取って点数の合計を返す *)
let point_sum lst = List.fold_right add_point lst 0;;

let test1 = point_sum students1 = 0;;
let test2 = point_sum students2 = 19;;
let test3 = point_sum students3 = 160;;

(* ex14.5 *)
let even =
  let f x = x mod 2 = 0 in
    List.filter f;;

let count_A lst =
  let f p = match p with
      { blood_type = "A" } -> true
    | _                    -> false
  in
    List.length (List.filter f lst)

let concat lst =
  let f a b = a ^ b in
    List.fold_right f lst "";;

let point_sum lst =
  let f a b = match a with
    { point = a_p } -> a_p + b
  in
    List.fold_right f lst 0;;

(* ex14.6 *)
(* person_tのリストと血液型(string)を受け取って指定した血液型の人数を返す *)
(* count: person_t list -> string -> int *)
let count lst ty =
  let f p = match p with
    { blood_type = bt } -> bt = ty
  in
    List.length (List.filter f lst);;

let test1 = count persons1 "A" = 0;;
let test2 = count persons2 "A" = 3;;

(* ex14.8 *)
(*
   整数を受け取り、二乗から1引いた数を返す
   無名関数で実装
 *)
let pow2_sub1 = fun x -> x * x - 1;;
let test1 = pow2_sub1 1 = 0;;
let test2 = pow2_sub1 2 = 3;;
let test3 = pow2_sub1 3 = 8;;

(* ex14.9 *)
let person1 = { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };;
let person2 = { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "C" };;
let person3 = { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };;
(*
  person_tを受け取って, 名前を返す
  無名関数を利用する
 *)
let get_person_name = fun p ->
  match p with
    { name = n; height = _; weight = _; birthday = _; blood_type = _; } -> n;;

let test1 = get_person_name person1 = "aaa";;
let test2 = get_person_name person2 = "bbb";;
let test3 = get_person_name person3 = "ccc";;

(* ex14.10 *)
let even = List.filter (fun x -> x mod 2 = 0);;
let count_A lst = List.length
  (
    List.filter
      (fun p -> match p with
          { blood_type = bt } -> bt = "A"
      )
      lst
  )

let concat lst = List.fold_right (fun a b -> a ^ b) lst "";;
let point_sum lst = List.fold_right (fun a b -> match a with { point = a_p } -> a_p + b) lst 0;;

(* ex14.4 *)
(* concatを一行で定義する *)
let concat lst = List.fold_right (^) lst "";;

(* ex14.15 *)
let rec enumerate n =
  if n = 0 then [] else n :: enumerate (n-1)
(*
  整数nを受け取って, 1からnまでの合計を求める
  enumerateと高階関数を使うこと
 *)
(* one_to_n: int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0;;

let test1 = one_to_n 0 = 0;;
let test2 = one_to_n 1 = 1;;
let test3 = one_to_n 5 = 15;;
let test4 = one_to_n 10 = 55;;
(* ex14.16 *)
(*
  整数nを受け取って
 *)
let fac n = List.fold_right ( * ) (enumerate n) 1;;

let test1 = fac 0 = 1;;
let test2 = fac 1 = 1;;
let test3 = fac 5 = 120;;
let test4 = fac 10 = 3628800;;
