(* ex10.1 *)
(* 昇順に並んだ整数のリストlstと整数nを受け取り, 昇順となる位置に挿入したリストを返す *)
(* insert: int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> if first < n
                     then first :: insert rest n
                     else n :: first :: rest;;

let test1 = insert [] 1 = [1];;
let test2 = insert [2; 3] 1 = [1; 2; 3];;
let test3 = insert [1; 3] 2 = [1; 2; 3];;
let test4 = insert [1; 2] 3 = [1; 2; 3];;

(* ex10.2 *)
(* リストlstを受け取って昇順に整列させたリストを返す(挿入法) *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest ->
      insert (ins_sort rest) first;;

let test1 = ins_sort [] = [];;
let test2 = ins_sort [1] = [1];;
let test3 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8];;


type student_t = {
  name: string;
  point: int;
  assesment: string;
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

(* ex10.2 *)
(* student_t型のリストlstとstudent_t型のstuを受け取って、pointを整列した位置に挿入する *)
(* student_insert: student_t list student_t -> student list *)
let rec student_insert lst stu = match lst with
    [] -> [stu]
  | ({ name = n; point = p; assesment = a } as first) :: rest ->
      if p < stu.point
      then first :: student_insert rest stu
      else stu :: first :: rest;;

(* student_t型のリストlstを受け取って、pointの順で整列したリストを返す *)
(* student_sort: student_t list -> student list *)
let rec student_sort lst = match lst with
    [] -> []
  | first :: rest -> student_insert (student_sort rest) first;;

let test1 = student_sort students1 = [];;
let test2 = student_sort students2 = [
  {name = "aa"; point = 19; assesment = "C"};
];;
let test3 = student_sort students3 = [
  {name = "cc"; point = 1; assesment = "C"};
  {name = "aa"; point = 59; assesment = "B"};
  {name = "bb"; point = 100; assesment = "A"};
];;

(* ex10.4 *)
type person_t = {
  name: string;
  height: float;
  weight: float;
  birthday: int * int;
  blood_type: string;
};;

let persons1 = [];;
let persons2 = [{ name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" }];;
let persons3 = [
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "C" };
  { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
];;

(* person_t型のリストlstとperson_t型の値personを受け取って名前順で整列した位置に挿入する *)
(* person_insert: person_t list -> person_t -> person_t list *)
let rec person_insert lst person = match lst with
    [] -> [person]
  | { name = n; } as first :: rest ->
      if person.name < n
      then person :: first :: rest
      else first :: person_insert rest person;;

(* person_t型のリストlstを受け取って, 名前順で整列したリストを返す *)
(* person_insert: person_t list -> person_t list *)
let rec person_sort lst = match lst with
    [] -> []
  | first :: rest ->
      person_insert (person_sort rest) first;;

let test1 = person_sort persons1 = [];;
let test2 = person_sort persons2 = [{ name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" }];;
let test3 = person_sort persons3 = [
  { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "C" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
];;

(* ex10.5 *)
(* student_t型のリストを受け取ってpointが最高のレコードを返す *)
let rec max_point_sutdent lst = match lst with
    [] -> {name = ""; point = min_int; assesment = "" }
  | { name = _; point = p; assesment = _ } as first :: rest ->
      match max_point_sutdent rest with
        { name = _; point = rest_max_point; assesment = _ } ->
          if p >= rest_max_point
          then first
          else max_point_sutdent rest;;

let test1 = max_point_sutdent students1 = {name = ""; point = min_int; assesment = "" };;
let test2 = max_point_sutdent students2 = {name = "aa"; point = 19; assesment = "C"};;
let test3 = max_point_sutdent students3 = {name = "bb"; point = 100; assesment = "A"};;

(* ex10.6 *)
(* student_t型のリストを受け取ってpointが最高のレコードを返す
   ただし、局所変数を用いて同じ計算を二度行わない
*)
let rec max_point_sutdent lst = match lst with
    [] -> {name = ""; point = min_int; assesment = "" }
  | { name = _; point = p; assesment = _ } as first :: rest ->
      let { name = _; point = rest_max_point; assesment = _ } as rest_max_student = max_point_sutdent rest in
      if p >= rest_max_point
      then first
      else rest_max_student;;

let test1 = max_point_sutdent students1 = {name = ""; point = min_int; assesment = "" };;
let test2 = max_point_sutdent students2 = {name = "aa"; point = 19; assesment = "C"};;
let test3 = max_point_sutdent students3 = {name = "bb"; point = 100; assesment = "A"};;

(* ex10.6 *)
let persons1 = [];;
let persons2 = [{ name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" }];;
let persons3 = [
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "B" };
  { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "B" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "O" };
  { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "O" };
];;

(* ex10.7 *)
(* person_t型のレコードのリストを受け取って, 各血液型の人数のtupleを返す *)
(* group_by_blood_type: person_t list -> int * int * int * int * int *)
(* int * int * int * int * int は (A, B, O, AB, other) *)
let rec group_by_blood_type lst = match lst with
    [] -> (0, 0, 0, 0, 0)
  | { name = _; blood_type = bt } :: rest ->
      let (a, b, o, ab, other) = group_by_blood_type rest in
        if bt = "A" then (a+1, b, o, ab, other)
        else if bt = "B" then (a, b+1, o, ab, other)
        else if bt = "O" then (a, b, o+1, ab, other)
        else if bt = "AB" then (a, b, o, ab+1, other)
        else (a, b, o, ab, other+1)

let test1 = group_by_blood_type persons1 = (0, 0, 0, 0, 0);;
let test2 = group_by_blood_type persons2 = (1, 0, 0, 0, 0);;
let test3 = group_by_blood_type persons3 = (2, 2, 2, 4, 0);;

(* ex10.8 *)
(* person_t型のレコードのリストを受け取って, 最も多い血液型を返す *)
(* max_blood_type: person_t list -> "string" *)
let rec max_blood_type lst = match lst with
    [] -> "same all"
    | _ ->
        let (a, b, o, ab, other) = group_by_blood_type lst in
        let max_value = max other (max (max a b) (max o ab)) in
          if max_value = a then "A"
          else if max_value = b then "B"
          else if max_value = o then "O"
          else if max_value = ab then "AB"
          else "same all"

let test1 = max_blood_type persons1 = "same all";;
let test2 = max_blood_type persons2 = "A";;
let test3 = max_blood_type persons3 = "AB";;

(* example 10.6 *)
(* 目的: 昇順に並んでいるリストlst1とlst2をマージする *)
(* merge: int list -> int list -> int list *)
let rec merge lst1 lst2 = match (lst1, lst2) with
    ([], []) -> []
    | ([], lst2) -> lst2
    | (lst1, []) -> lst1
    | (first1 :: rest1, first2 :: rest2) ->
        if first1 < first2
        then first1 :: merge rest1 lst2
        else first2 :: merge lst1 rest2

let test1 = merge [] [] = [];;
let test2 = merge [] [1; 2] = [1; 2];;
let test3 = merge [1; 2] [] = [1; 2];;
let test4 = merge [1; 3] [2; 4] = [1; 2; 3; 4];;
let test5 = merge [2; 4] [1; 3] = [1; 2; 3; 4];;
let test6 = merge [1; 4] [1; 3] = [1; 1; 3; 4];;

(* ex10.9 *)
(* リストlst1, lst2を受け取って 長さが同じかどうかを判定する *)
(* equal_length: a' list -> a' list -> bool *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([], []) -> true
  | (xs, []) -> false
  | ([], xs) -> false
  | (first1 :: rest1, first2 :: rest2) -> equal_length rest1 rest2

let test1 = equal_length [] [] = true;;
let test2 = equal_length [] [1; 2] = false;;
let test3 = equal_length [1; 2] [] = false;;
let test4 = equal_length [1; 3] [2; 4] = true;;
let test5 = equal_length [2; 4] [1; 3; 4] = false;;
