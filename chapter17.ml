type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t;;

(* ex17.5 *)
(* tree_tを受け取り節や葉の値を2倍したtree_tを返す *)
(* tree_double: tree_t -> tree_t *)
let rec tree_double t = match t with
    Empty -> Empty
  | Leaf (v) -> Leaf (2 * v)
  | Node (lt, v, rt) -> Node (tree_double lt, 2 * v, tree_double rt);;

(* test data *)
let tree1 = Empty;;
let tree2 = Leaf (3);;
let tree3 = Node (tree1, 4, tree2);;
let tree4 = Node (tree2, 5, tree3);;

(* tests *)
let test1 = tree_double tree1 = Empty;;
let test2 = tree_double tree2 = Leaf (6);;
let test3 = tree_double tree3 = Node (Empty, 8, Leaf (6));;
let test4 = tree_double tree4 = Node (Leaf (6), 10, Node (Empty, 8, Leaf (6)));;

(* ex17.6 *)
(* int -> int の関数とtree_tを受け取って、節や葉にfを適用する *)
(* tree_map: (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
    Empty -> Empty
  | Leaf (v) -> Leaf (f v)
  | Node (lt, v, rt) -> Node (tree_map f lt, f v, tree_map f rt);;

(* tests *)
let test1 = tree_map (fun x -> 2 * x) tree1 = Empty;;
let test2 = tree_map (fun x -> 2 * x) tree2 = Leaf (6);;
let test3 = tree_map (fun x -> 2 * x) tree3 = Node (Empty, 8, Leaf (6));;
let test4 = tree_map (fun x -> 2 * x) tree4 = Node (Leaf (6), 10, Node (Empty, 8, Leaf (6)));;

(* ex17.7 *)
(* tree_tを受け取って, 節と葉の数を返す *)
(* tree_length: tree_t -> int *)
let rec tree_length t = match t with
    Empty -> 0
  | Leaf (_) -> 1
  | Node (lt, _, rt) -> tree_length lt + 1 + tree_length rt;;

let test1 = tree_length tree1 = 0;;
let test2 = tree_length tree2 = 1;;
let test3 = tree_length tree3 = 2;;
let test4 = tree_length tree4 = 4;;

(* ex17.8 *)
(* tree_tを受け取って, 深さを返す *)
(* tree_depth: tree_t -> int *)
let rec tree_depth t = match t with
    Empty -> 0
  | Leaf (_) -> 0
  | Node (lt, _, rt) ->
      let lt_depth = tree_depth lt in
      let rt_depth = tree_depth rt in
      (if lt_depth > rt_depth then lt_depth else rt_depth) + 1;;

(* tests *)
let test1 = tree_depth tree1 = 0;;
let test2 = tree_depth tree2 = 0;;
let test3 = tree_depth tree3 = 1;;
let test4 = tree_depth tree4 = 2;;

(* example 17.4 *)
(* 二分探索 *)
(* search: tree_t -> int -> bool *)
let rec search tree data = match tree with
    Empty -> false
  | Leaf (v) -> v = data
  | Node (lt, v, rt) ->
      if v = data then true
      else if data < v then search lt data
      else search rt data;;

(* 二分探索木に値を挿入 *)
(* : tree_t -> int -> tree_t *)
let rec insert_tree tree data = match tree with
    Empty -> Leaf (data)
  | Leaf (v) ->
      if v = data then Leaf (v)
      else if v < data then Node (Leaf (v), data, Empty)
      else Node (Empty, data, Leaf (v))
  | Node (lt, v, rt) ->
      if v = data then Node (lt, v, rt)
      else if data < v then Node (insert_tree lt data, v, rt)
      else Node (lt, v, insert_tree lt data);;

(* ex17.9 *)
type 'a pol_tree_t = Empty
                | Leaf of 'a
                | Node of 'a pol_tree_t * 'a * 'a pol_tree_t;;

(* 節と葉のすべての値を足し合わせる *)
(* sum_tree: int tree -> int *)
(* 0と+を使っているのでint treeになる *)
let rec sum_tree t = match t with
    Empty -> 0
  | Leaf (v) -> v
  | Node (lt, v, rt) -> sum_tree lt + v + sum_tree rt;;

let tree1 = Empty;;
let tree2 = Leaf (3);;
let tree3 = Node (tree1, 4, tree2);;
let tree4 = Node (tree2, 5, tree3);;

let test1 = sum_tree tree1 = 0;;
let test2 = sum_tree tree2 = 3;;
let test3 = sum_tree tree3 = 7;;
let test4 = sum_tree tree4 = 15;;

