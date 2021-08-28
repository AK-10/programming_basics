(* ex16.1 *)
(* 整数のリストを受け取ってそれまでの合計からなるリストを返す *)
(* sum_list int list -> int list *)
let sum_list lst =
  (* sums: int list -> int -> int list *)
  let rec sums lst acc = match lst with
      [] -> []
    | first :: rest ->
        let n = first + acc in
          n :: sums rest n
  in
    sums lst 0

let test1 = sum_list [] = [];;
let test2 = sum_list [1] = [1];;
let test3 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10];;

(* example16.3 *)
(* リストを受け取って逆順のリストを返す *)
(* reverse: 'a list -> 'a list *)
let reverse lst =
  (* rev: 'a list -> 'a list -> 'a list *)
  let rec rev lst result = match lst with
      [] -> result
    | first :: rest -> rev rest (first :: result)
  in
    rev lst [];;

let test1 = reverse [] = [];;
let test2 = reverse [1] = [1];;
let test3 = reverse [3; 2; 1; 4] = [4; 1; 2; 3];;

(* ex16.2 *)
(* 関数f, 初期値init, リストlstを受け取ってinitからはじめてlstの要素を左から順に施しこむ関数fold_left *)
(* fold_left: ('a -> 'b -> 'a) -> 'a -> 'c list -> 'a *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest

let test1 = fold_left (+) 0 [] = 0;;
let test2 = fold_left (+) 0 [1] = 1;;
let test3 = fold_left (+) 0 [3; 2; 1; 4] = 10;;

