type person_t = {
  name: string;
  height: float;
  weight: float;
  birthday: int * int;
  blood_type: string;
};;

(* ex13.1 *)
(* person_tのリストと血液型の文字列を受け取り, 該当する人の数を返す *)
(* person_t list -> string -> int *)
let rec count_blood persons blood_type = match persons with
    [] -> 0
  | { blood_type = bt; } :: rest ->
      if bt = blood_type then
        1 + count_blood rest blood_type
      else
        count_blood rest blood_type;;

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

let test1 = count_blood persons1 "A" = 0;;
let test2 = count_blood persons2 "A" = 3;;
let test3 = count_blood persons2 "B" = 2;;
let test4 = count_blood persons2 "O" = 1;;

(* ex13.2 *)
(* person_tを受け取って,名前を返す *)
(* person_name: person_t -> string *)
let person_name p = match p with
  { name = n } -> n;;
(* person_t のリストを受け取ってそれぞれの名前を返す *)
(* person_names: person_t list -> string list *)
let person_names persons = List.map person_name persons;;

let test1 = person_names persons1 = [];;
let test2 = person_names persons2 = [
  "aaa";
  "bbb";
  "ccc";
  "eee";
  "kkk";
  "ooo";
  "www";
];;

(* ex13.3 *)
(* 1. 'a -> 'a な関数 *)
let id x = x;;
(* 2. 'a -> 'b -> 'a な関数 *)
let fun2 x y = x;;
(* 3. 'a -> 'b -> 'b な関数 *)
let fun3 x y = y;;
(* 4. 'a -> ('a -> 'b) -> 'b な関数 *)
let fun4 x f = f x;;
(* 5. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c な関数 *)
let fun5 f g x = g (f x);;

(* ex13.4 *)
(* 関数を２つ受け取って２つの関数を合成した関数を返す *)
(* compose: ('a -> 'b) -> ('c -> 'a) -> 'c-> 'b *)
let compose f g =
  let h x = f (g x) in h;;

let time2 x = x * 2;;
let add3 x = x + 3;;

let test1 = (compose time2 add3) 4 = 14;;

(* ex13.5 *)
(* twiceにtwice自身を渡して twice twice とすることができる.
   ここで帰ってくる関数はどのような関数か?
 *)
(* 型: ('a -> 'a) -> 'a -> 'a *)
(*
let g x = f (f x) in g;;
twice_twice x = twice (twice x)
              = f (f (f (f x)))

  2 * 2で4回関数を適用する
 *)
let twice f =
  let g x = f (f x) in g;;

let twice_twice = twice twice;;

