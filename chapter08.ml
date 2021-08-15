(* ex8.1 *)
type book_t = {
  author: string;
  publisher: string;
  price: int;
  isbn: string;
};;

let book1 = { author = "a"; publisher = "b"; price = 100; isbn = "142421d" };;
let book2 = { author = "c"; publisher = "d"; price = 1100; isbn = "adbdkljf" };;
let book3 = { author = "e"; publisher = "f"; price = 1300; isbn = "wasd" };;

(* ex8.2 *)
type okozukai_t = {
  product: string;
  price: int;
  place: string;
  date: string;
};;

let okozukai1 = { product = "aaa"; price = 100; place = "hawaii"; date = "2021-01-01" };;
let okozukai2 = { product = "dfdklj"; price = 2100; place = "newyork"; date = "2021-11-01" };;
let okozukai3 = { product = "d"; price = 1100; place = "hakata"; date = "2021-11-02" };;

(* ex8.3 *)
type person_t = {
  name: string;
  height: float;
  weight: float;
  birthday: int * int;
  blood_type: string;
};;

let person1 = { name = "aaa"; height = 1.44; weight = 59.1; birthday = (1,2); blood_type = "A" };;
let person2 = { name = "bbb"; height = 1.90; weight = 79.1; birthday = (3,1); blood_type = "C" };;
let person3 = { name = "ccc"; height = 1.70; weight = 59.1; birthday = (8,3); blood_type = "AB" };;

(* ex8.4 *)
(* 人のデータpersonを受け取り, 「xxさんの血液型はyyです」を表示する *)
(* ketsueki_hyoji: person_t -> string *)
let ketsueki_hyoji person = match person with
    { name = n; height = h; weight = w; birthday = bd; blood_type = bt; } -> n ^ "さんの血液型は" ^ bt ^ "です";;


let test1 =
  ketsueki_hyoji { name = "x"; height = 1.32; weight = 35.1; birthday = (9,12); blood_type = "A"; } = "xさんの血液型はAです";;
