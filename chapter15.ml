(* example 15.1 *)
(* lstの中からnより小さい要素のみ取り出す *)
(* take_less: int -> int list -> int list *)
let rec take_less n lst = match lst with
    []            -> []
  | first :: rest ->
      if first < n then
        first :: take_less n rest
      else
        take_less n rest

(* lstの中からnより大きい要素のみ取り出す *)
(* take_less: int -> int list -> int list *)
let rec take_greater n lst = match lst with
    []            -> []
  | first :: rest ->
      if first >= n then
        first :: take_greater n rest
      else
        take_greater n rest

(* 受け取ったlstをクイックソートを使って昇順に整列する *)
(* quick_sort: int list -> int list *)
let rec quick_sort lst = match lst with
    []            -> []
  | first :: rest -> quick_sort (take_less first rest) @ [first] @ quick_sort (take_greater first rest)

let test1 = quick_sort [] = [];;
let test2 = quick_sort [1] = [1];;
let test3 = quick_sort [1; 2] = [1; 2];;
let test4 = quick_sort [2; 1] = [1; 2];;
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9];;
let test6 = quick_sort [5; 5; 9; 8; 2; 3] = [2; 3; 5; 5; 8; 9];;

(* 受け取ったlstをクイックソートを使って昇順に整列する *)
(* 局所関数、filterを用いる *)
let rec quick_sort2 lst =
  let take n lst p = List.filter (fun item -> p item n) lst in
  let take_less n lst = take n lst (<) in
  let take_greater n lst = take n lst (>) in
  match lst with
      [] -> []
    | first :: rest -> quick_sort2 (take_less first rest) @ [first] @ quick_sort2 (take_greater first rest)

let test1 = quick_sort2 [] = [];;
let test2 = quick_sort2 [1] = [1];;
let test3 = quick_sort2 [1; 2] = [1; 2];;
let test4 = quick_sort2 [2; 1] = [1; 2];;
let test5 = quick_sort2 [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9];;

(* ex15.1 *)
(*
   上記のquick_sortは正しく動作しない。どんな場合か？
   またどのように修正すればよいか
 *)
(*
   A: ピボットとして選ばれた値(今回はfirst)と同じ値が含まれているとき
      take_less, take_greaterのどちらかで=の場合も取るようにすればよい
 *)

(* ex15.2 *)
(* 
   ２つの自然数m, nを受け取って最大公約数を返す
   ユークリッドの互除法を用いる
   (1) n = 0 ならば最大公約数はm
   (2) そうでなければ、nと「mをnで割った余り」の最大公約数が求める最大公約数
 *)
(* gcd: int -> int -> int *)
let rec gcd m n =
  if m = 0 then
    n
  else if n = 0 then
    m
  else
    gcd n (m mod n)

let test1 = gcd 0 1 = 1
let test2 = gcd 1 0 = 1
let test3 = gcd 6 2 = 2
let test4 = gcd 100 45 = 5
let test5 = gcd 3355 2379 = 61

(* 停止性についての議論 *)
(*
  今回の自明なケースはm, n のいずれかが0のとき
  elseでの計算について見ると、`m mod n`から求まる値はnより小さくなる
  余り1になり、次の再帰呼び出しで1で割ることであまりが0になる
  これにより確実に停止する
 *)

(* ex15.3 *)

(* 2からn までの自然数のリストを返す *)
(* gen_list: int -> int list *)
let gen_list n =
  let rec loop i =
    if i <= n then
      i :: loop (i + 1)
    else
      []
  in loop 2

(* 自然数2..nのリストを受け取って、2以上n以下の素数のリストを返す *)
(* sieve: int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest -> first :: sieve (List.filter (fun x -> x mod first <> 0) rest)

let test1 = sieve [] = [];;
let test2 = sieve [2] = [2];;
let test3 = sieve [2; 3; 4; 5; 6; 7] = [2; 3; 5; 7];;

(* 停止性についての議論 *)
(*
  再帰のたびに対象のリストは小さくなるため, いずれ[]になり停止する
 *)

(*
  自然数n以下の素数を求める
  エラトステネスの篩を用いること
  (1) 2からnまでの自然数のリストを作る
  (2) リストの戦闘の要素は素数である(2なので)
  (3) リストの残りの中からリストの戦闘の要素で割り切れるものは取り除く
  (4) (2)以降をリストが空になるまで繰り返す
 *)
(* prime: int -> int list *)
let prime n = sieve (gen_list n)
let test1 = prime 1 = [];;
let test2 = prime 2 = [2];;
let test3 = prime 50 = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47];;

