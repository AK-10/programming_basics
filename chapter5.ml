(* 時間tを受け取って、午前か午後かを返す *)
(* jikan: int -> string *)
let jikan t =
  if (t mod 24) < 12 then "am"
                   else "pm";;

let test0 = jikan 0 = "am";;
let test1 = jikan 12 = "pm";;
let test2 = jikan 24 = "am";;
let test3 = jikan 56 = "am";;

(* 月month, 日付dayを受け取って星座を返す *)
(* seiza: int -> int -> string *)
let seiza month day =
  if month = 1 then if 1 <= day && day <= 19 then "山羊座"
		    else if 20 <= day && day <= 31 then "水瓶座"
		    else "なし"
  else if month = 2 then if 1 <= day && day <= 18 then "水瓶座"
		    else if 19 <= day && day <= 29 then "魚座"
		    else "なし"
  else if month = 3 then if 1 <= day && day <= 20 then "魚座"
        else if 21 <= day && day <= 31 then "牡羊座"
		    else "なし"
  else if month = 4 then if 1 <= day && day <= 19 then "牡羊座"
		    else if 20 <= day && day <= 30 then "牡牛座"
		    else "なし"
  else if month = 5 then if 1 <= day && day <= 20 then "牡牛座"
		    else if 21 <= day && day <= 31 then "双子座"
		    else "なし"
  else if month = 6 then if 1 <= day && day <= 21 then "双子座"
		    else if 22 <= day && day <= 30 then "蟹座"
		    else "なし"
  else if month = 7 then if 1 <= day && day <= 22 then "蟹座"
		    else if 23 <= day && day <= 31 then "獅子座"
		    else "なし"
  else if month = 8 then if 1 <= day && day <= 22 then "獅子座"
		    else if 23 <= day && day <= 31 then "乙女座"
		    else "なし"
  else if month = 9 then if 1 <= day && day <= 22 then "乙女座"
		    else if 23 <= day && day <= 30 then "天秤座"
		    else "なし"
  else if month = 10 then if 1 <= day && day <= 23 then "天秤座"
		     else if 24 <= day && day <= 31 then "蠍座"
		     else "なし"
  else if month = 11 then if 1 <= day && day <= 21 then "蠍座"
		     else if 22 <= day && day <= 30 then "射手座"
		     else "なし"
  else if month = 12 then if 1 <= day && day <= 21 then "射手座"
		     else if 22 <= day && day <= 31 then "山羊座"
		     else "なし"
  else "なし"

let test1 = seiza 6 11 = "双子座"
let test2 = seiza 6 30 = "蟹座"
let test3 = seiza 9 17 = "乙女座"
let test4 = seiza 10 7 = "天秤座"

(* ax^2 + bx +c = 0 のa,b,cを受け取り, 判別式の値を返す *)
(* hanbetsusiki: float -> float -> float -> float *)
let hanbetsusiki a b c = b ** 2.0 -. 4.0 *. a *.c ;;

let test0 = hanbetsusiki 1. 3. 2. = 1.;;
let test1 = hanbetsusiki 1. 5. 4. = 9.;;
let test2= hanbetsusiki 2. (-4.) 2. = 0.;;

(* ax^2 + bx +c = 0 のa,b,cを受け取り, 解の個数を返す *)
(* hanbetsusiki: float -> float -> float -> int *)
let kai_no_kosuu a b c = if hanbetsusiki a b c > 0.
                         then 2
                         else if hanbetsusiki a b c = 0.
                         then 1
                         else 0;;

let test0 = kai_no_kosuu 0. 0. 0. = 1;;
let test1 = kai_no_kosuu 1. 3. 2. = 2;;
let test2 = kai_no_kosuu 4. 1. 1. = 0;;

(* hanbetsusiki: float -> float -> float -> bool *)
let kyosuukai a b c = hanbetsusiki a b c < 0.;;

let test0 = kyosuukai 0. 0. 0. = false;;
let test1 = kyosuukai 1. 3. 2. = false;;
let test2 = kyosuukai 4. 1. 1. = true;;
(* 身長h, 体重w 受け取って bmiを返す *)
let bmi h w = w /. h ** 2.;;

(* 身長h, 体重w 受け取って bmiを返す *)
let taikei h w = if bmi h w < 18.5 then "slim"
                 else if 18.5 <= bmi h w && bmi h w < 25. then "normal"
                 else if 25. <= bmi h w && bmi h w < 30. then "fat"
                 else "super fat";;

let test0 = taikei 1.70 50. = "slim";;
let test1 = taikei 1.70 62. = "normal";;
let test2 = taikei 1.70 80. = "fat";;
let test3 = taikei 1.70 200. = "super fat";;
