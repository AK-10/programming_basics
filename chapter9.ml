(* ex9.1 *)
let seasons = "春" :: "夏" :: "秋" :: "冬" :: [];;

(* ex9.2 *)
type person_t = {
  name: string;
  height: float;
  weight: float;
  birthday: int * int;
  blood_type: string;
};;

let persons =
  { name = "あああ"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" } ::
  { name = "いいい"; height = 1.29; weight = 98.1; birthday = (5,2); blood_type = "B" } ::
  { name = "ううう"; height = 1.90; weight = 89.1; birthday = (1,28); blood_type = "AB" } ::
  [];;

(* ex9.3 *)
let seasons2 = ["春"; "夏"; "秋"; "冬"];;

(* ex9.4 *)
(* 'a list は
  - []            空リスト あるいは
  - first :: rest 最初の要素が first で残りのリストが rest (restが自己参照)
 *)
(* リストlstを受け取って、その長さを返す *)
(* length: 'a list -> int *)
let rec length lst = match lst with
    []        -> 0
  | _ :: rest -> length rest + 1;;

let test1 = length [] = 0;;
let test2 = length [1] = 1;;
let test3 = length [1; 3; 4; 5] = 4;;

(* ex9.5 *)
(* リストlstを受け取って、偶数のみを含むリストを返す *)
(* even: int list -> int list *)
let rec even lst = match lst with
    []            -> []
  | first :: rest -> if first mod 2 = 0
                     then first :: even rest
                     else even rest;;

let test1 = even [] = [];;
let test2 = even [1] = [];;
let test3 = even [2] = [2];;
let test4 = even [1; 2; 3; 4] = [2; 4];;

(* ex9.6 *)
(* 文字列のリストlstを受け取って、前から順に結合した文字列を返す *)
(* concat: string list -> string *)
let rec concat lst = match lst with
    []            -> ""
  | first :: rest -> first ^ concat rest;;

let test1 = concat [] = "";;
let test2 = concat ["a"; "b"; "c";] = "abc";;
let test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬";;

(* ex9.7 *)
(* person_t型のレコードのリストlstを受け取って、血液型がA型の人の数を返す *)
(* count_blood_type_a: person_t list -> int *)
let rec count_blood_type_a lst = match lst with
    [] -> 0
  | { name = _; height = _; weight = _; birthday = _; blood_type = "A" } :: rest -> 1 + count_blood_type_a rest
  | { name = _; height = _; weight = _; birthday = _; blood_type = _ } :: rest -> count_blood_type_a rest;;

let persons1 = [];;
let persons2 = [
  { name = "あああ"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
];;
let persons3 = [
  { name = "あああ"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "いいい"; height = 1.29; weight = 98.1; birthday = (5,2); blood_type = "B" };
  { name = "ううう"; height = 1.90; weight = 89.1; birthday = (1,28); blood_type = "AB" };
  { name = "ううう"; height = 1.90; weight = 89.1; birthday = (1,28); blood_type = "O" };
  { name = "ううう"; height = 1.90; weight = 89.1; birthday = (1,28); blood_type = "A" };
];;

let test1 = count_blood_type_a persons1 = 0;;
let test2 = count_blood_type_a persons2 = 1;;
let test3 = count_blood_type_a persons3 = 2;;

(* ex9.8 *)
(* 月、日のペアを受け取って乙女座かどうかを返す *)
(* 乙女座は 8/23 - 9/22 *)
(* is_otomeza: (int * int) -> bool *)
let is_otomeza (month, day) =
  if month = 8 && 23 <= day && day <= 31 then true
  else if month = 9 && 1 < day && day <= 22 then true
  else false;;

(* person_t型のレコードのリストを受け取って, 乙女座の人の名前のみからなるリストを返す *)
(* otomeza: person_t list -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | { name = n; height = _; weight = _; birthday = bd; blood_type = _ } :: rest ->
      if is_otomeza bd
      then n :: otomeza rest
      else otomeza rest;;

let persons1 = [];;
let persons2 = [
  { name = "あああ"; height = 1.44; weight = 59.1; birthday = (8,29); blood_type = "A" };
];;
let persons3 = [
  { name = "あああ"; height = 1.44; weight = 59.1; birthday = (8,23); blood_type = "A" };
  { name = "いいい"; height = 1.29; weight = 98.1; birthday = (8,2); blood_type = "B" };
  { name = "ううう"; height = 1.90; weight = 89.1; birthday = (9,10); blood_type = "AB" };
  { name = "えええ"; height = 1.90; weight = 89.1; birthday = (1,28); blood_type = "O" };
  { name = "おおお"; height = 1.90; weight = 89.1; birthday = (9,22); blood_type = "A" };
];;

let test1 = otomeza persons1 = [];;
let test2 = otomeza persons2 = ["あああ"];;
let test3 = otomeza persons3 = ["あああ"; "ううう"; "おおお"];;

